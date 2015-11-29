package server;

import lockmanager.*;
import common.*;
import common.operations.*;
import transactionmanager.Transaction;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public abstract class DatabaseResourceManager implements ResourceManager {
    public static final int DEFAULT_TRANSACTION_TTL = 90;

    private static int nextCustomerId = 0;

    private static synchronized int getNextCustomerId() {
        return nextCustomerId++;
    }
    protected final ResourceManager middleware;
    protected final Data mainData;
    protected final LockManager lockManager;
    protected final TransactionDataStore mainDataStore;
    protected final HashMap<Integer, TransactionDataStore> transactions;
    protected final HashMap<Integer, TransactionDataStore> preparedTransactions;
    protected final HashMap<Integer, Integer> ttls;

    protected final ScheduledExecutorService ttlScheduler;

    private final String dbname;

    final SecurePersistenceLayer<Data>
        dataPersistenceLayer;

    final SecurePersistenceLayer<TransactionList>
        transactionListPersistenceLayer;

    public DatabaseResourceManager(String dbname, ResourceManager middleware) {
        lockManager = new LockManager();
        this.middleware = middleware;
        transactions = new HashMap<Integer, TransactionDataStore>();
        mainData = new Data(String.format("main-%s.dat", dbname));
        preparedTransactions = new HashMap<Integer, TransactionDataStore>();
        mainDataStore = new TransactionDataStore(
                dbname,
                TransactionOperation.NO_TRANSACTION,
                null,
                mainData);
        this.dbname = dbname;

        String dataPaths[] = new String[] {
            String.format("main-%s-0.dat", dbname),
            String.format("main-%s-1.dat", dbname)
        };

        String transactionListPaths[] = new String[] {
            String.format("txns-%s-0.dat", dbname),
            String.format("txns-%s-1.dat", dbname)
        };

        dataPersistenceLayer =
            new SecurePersistenceLayer<Data>(Arrays.asList(dataPaths));

        transactionListPersistenceLayer =
            new SecurePersistenceLayer<TransactionList>(
                    Arrays.asList(transactionListPaths));

        ttls = new HashMap<Integer, Integer>();
        ttlScheduler = Executors.newSingleThreadScheduledExecutor();

        recoverData();
    }

    private void scheduleTtlCheck(int transactionId, long delta) {
        Trace.info(
                String.format(
                    "Scheduling TTL check for transaction %d in %d seconds.",
                    transactionId,
                    delta / 1000
                )
        );

        ttlScheduler.schedule(
                new TtlChecker(transactionId),
                delta / 1000,
                TimeUnit.SECONDS
        );
    }

    protected void touch(int transactionId) {
        if(transactionId == -1)
            return;

        synchronized(ttls) {
            ttls.put(transactionId, DEFAULT_TRANSACTION_TTL);
        }
    }

    protected synchronized void mergeData(Map<String, ItemGroup> data) {
        mainData.putAll(data);
    }

    protected TransactionDataStore getTransactionData(int id) {
        if(id == TransactionOperation.NO_TRANSACTION)
            return mainDataStore;

        final TransactionDataStore txData = transactions.get(id);

        if(txData == null)
            throw UncheckedThrow.throwUnchecked(
                    new NoSuchTransactionException(id)
            );

        return txData;
    }

    public String getDatabaseName() {
        return dbname;
    }

    private TransactionList getTransactionList() {
        final TransactionList txns = new TransactionList();
        for(final Integer id : preparedTransactions.keySet())
            txns.add(id);
        return txns;
    }

    private TransactionList getPreparedTransactionList() {
        final TransactionList txns = new TransactionList();
        for(final Integer id: preparedTransactions.keySet())
            txns.add(id);
        return txns;
    }

    private void recoverData() {
        Data recoveredData = null;
        TransactionList t  = null;
        try{
            recoveredData = dataPersistenceLayer.load();
            t = transactionListPersistenceLayer.load();
        } catch(Exception e) {
            UncheckedThrow.throwUnchecked(e);
        }

        if(recoveredData != null)
            mainData.putAll(recoveredData);
        if(t == null){
            Trace.info("Transaction List is null, recovery complete"); 
            return;
        }

        TransactionList modifiedTransactionList = t;
        SecurePersistenceLayer<Data> transactionDataPersistenceLayer;
        Data transactionData = null;
        for(int i=0; i < t.size(); i++) {
            try{
                Trace.info(String.format("Recovering transaction with id %d", i));
                ArrayList<String> path = new ArrayList<String>();
                path.add(TransactionDataStore.getTransactionFileName(dbname,t.get(i)));
                transactionDataPersistenceLayer = new SecurePersistenceLayer<Data>(path);
                transactionData = transactionDataPersistenceLayer.load();

            }catch(Exception e) {
                UncheckedThrow.throwUnchecked(e);
            }

            if(transactionData == null){
                continue;
            } else {
                final Transaction.State status = middleware.getTransactionStatus(i);
                if(status == Transaction.State.COMMITTED){ //if commited - check with the middleware and work on result
                    mainData.putAll(transactionData);
                } else if(status == Transaction.State.ABORTED){
                } else if(status == Transaction.State.PREPARED){
                    lockManager.incrementPreparedTransactionCount(); 
                    preparedTransactions.put(t.get(i),new TransactionDataStore(t.get(i),mainData,transactionData,null)); 
                }
                else {

                    throw new RuntimeException(String.format("Transaction Status invariant violated %s", status.toString()));
                }

                modifiedTransactionList.remove(i);
            }

        }
        try{
            transactionListPersistenceLayer.persist(modifiedTransactionList);
            dataPersistenceLayer.persist(mainData);
        }catch(IOException e) {
            UncheckedThrow.throwUnchecked(e);
        }
    }

    // Flight operations //

    // Create a new flight, or add seats to existing flight.
    // Note: if flightPrice <= 0 and the flight already exists, it maintains
    // its current price.
    @Override
    public boolean addFlight(int id, int flightNumber,
                             int numSeats, int flightPrice) {
        Trace.info("RM::addFlight(" + id + ", " + flightNumber
                + ", $" + flightPrice + ", " + numSeats + ") called.");
        touch(id);
        return new AddFlightOperation(
                getTransactionData(id),
                id,
                flightNumber,
                numSeats,
                flightPrice)
            .invoke();
    }

    @Override
    public boolean deleteFlight(int id, int flightNumber) {
        Trace.info(
                String.format(
                    "RM::deleteFlight(%d, %d)",
                    id,
                    flightNumber
                )
        );
        touch(id);
		return new DeleteFlightOperation(
                getTransactionData(id),
                id,
                flightNumber)
            .invoke();
    }

    // Returns the number of empty seats on this flight.
    @Override
    public int queryFlight(int id, int flightNumber) {
        Trace.info(
                String.format(
                    "RM::queryFlight(%d, %d)",
                    id,
                    flightNumber
                )
        );
        touch(id);
        return new QueryFlightOperation(
            getTransactionData(id),
            id,
            flightNumber)
        .invoke();
    }

    // Returns price of this flight.
    public int queryFlightPrice(int id, int flightNumber) {
        Trace.info(
                String.format(
                    "RM::queryFlightPrice(%d, %d)",
                    id,
                    flightNumber
                )
        );
        touch(id);
        return new QueryFlightPriceOperation(
            getTransactionData(id),
            id,
            flightNumber)
        .invoke();
    }

    // Car operations //

    // Create a new car location or add cars to an existing location.
    // Note: if price <= 0 and the car location already exists, it maintains
    // its current price.
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
        Trace.info("RM::addCars(" + id + ", " + location + ", "
                + numCars + ", $" + carPrice + ") called.");

        touch(id);
        return new AddCarsOperation(
            getTransactionData(id),
            id,
            location,
            numCars,
            carPrice)
        .invoke();
    }

    // Delete cars from a location.
    @Override
    public boolean deleteCars(int id, String location) {
        Trace.info(
                String.format(
                    "RM::deleteCars(%d, %s)",
                    id,
                    location
                )
        );
        touch(id);
        return new DeleteCarsOperation(
            getTransactionData(id),
            id,
            location)
        .invoke();
    }

    // Returns the number of cars available at a location.
    @Override
    public int queryCars(int id, String location) {
        Trace.info(
                String.format(
                    "RM::queryCars(%d, %s)",
                    id,
                    location
                )
        );

        touch(id);
        return new QueryCarsOperation(
            getTransactionData(id),
            id,
            location)
        .invoke();
    }

    // Returns price of cars at this location.
    @Override
    public int queryCarsPrice(int id, String location) {
        Trace.info(
                String.format(
                    "RM::queryCarsPrice(%d, %s)",
                    id,
                    location
                )
        );
        touch(id);
        return new QueryCarsPriceOperation(
            getTransactionData(id),
            id,
            location)
        .invoke();
    }

    // Room operations //

    // Create a new room location or add rooms to an existing location.
    // Note: if price <= 0 and the room location already exists, it maintains
    // its current price.
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice) {
        Trace.info(
                String.format(
                    "RM::addRooms(%d, %s, %d, $%d)",
                    id,
                    location,
                    numRooms,
                    roomPrice
                )
        );
		
        touch(id);
		return new AddRoomsOperation(
            getTransactionData(id),
            id,
            location,
            numRooms,
            roomPrice)
        .invoke();
    }

    // Delete rooms from a location.
    @Override
    public boolean deleteRooms(int id, String location) {
        Trace.info(
                String.format(
                    "RM::deleteRooms(%d, %s)",
                    id,
                    location
                )
        );
	
        touch(id);
	    return new DeleteRoomsOperation(
            getTransactionData(id),
            id,
            location)
        .invoke();
    }

    // Returns the number of rooms available at a location.
    @Override
    public int queryRooms(int id, String location) {
        Trace.info(
                String.format(
                    "RM::queryRooms(%d, %s)",
                    id,
                    location
                )
        );
		
        touch(id);
        return new QueryRoomOperation(
            getTransactionData(id),
            id,
            location)
        .invoke();
    }

    // Returns room price at this location.
    @Override
    public int queryRoomsPrice(int id, String location) {
        Trace.info(
                String.format(
                    "RM::queryRoomsPrice(%d, %s)",
                    id,
                    location
                )
        );
		
        touch(id);
        return new QueryRoomPriceOperation(
            getTransactionData(id),
            id,
            location)
        .invoke();
    }

    @Override //intercepted in middleware
    public int newCustomer(int id) {
        Trace.info(
                String.format(
                    "RM::newCustomer(%d)",
                    id
                )
        );

        int customerId = getNextCustomerId();
        boolean result = newCustomerId(id, customerId);
        if(result)
            return customerId;
        else
            throw new RuntimeException(
                    "Customer " + customerId + " already exists.");
    }

    @Override
    public boolean newCustomerId(int id, int customerId) {
        Trace.info(
                String.format(
                    "RM::newCustomerId(%d, %d)",
                    id,
                    customerId
                )
        );

        touch(id);
        return new NewCustomerIdOperation(
            getTransactionData(id),
            id,
            customerId)
        .invoke();
    }

    @Override
    public boolean doesCustomerExist(int id, int customerId) {
        Trace.info(
                String.format(
                    "RM::doesCustomerExist(%d, %d)",
                    id,
                    customerId
                )
        );

        touch(id);
        return new DoesCustomerExistOperation(
            getTransactionData(id),
            id,
            customerId)
        .invoke();
    }

    // Add flight reservation to this customer.
    @Override
    public boolean reserveFlight(int id, int customerId, int flightNumber) {
        Trace.info(
                String.format(
                    "RM::reserveFlight(%d, %d, %d)",
                    id,
                    customerId,
                    flightNumber
                )
        );
        touch(id);
		return new ReserveFlightOperation(
                getTransactionData(id),
                id,
                customerId,
                flightNumber)
            .invoke();
    }

    // Add car reservation to this customer.
    @Override
    public boolean reserveCar(int id, int customerId, String location) {
        Trace.info(
                String.format(
                    "RM::reserveCar(%d, %d, %s)",
                    id,
                    customerId,
                    location
                )
        );

        touch(id);
        return new ReserveCarOperation(
                getTransactionData(id),
                id,
                customerId,
                location)
            .invoke();
    }

    // Add room reservation to this customer.
    @Override
    public boolean reserveRoom(int id, int customerId, String location) {
        Trace.info(
                String.format(
                    "RM::reserveRoom(%d, %d, %s)",
                    id,
                    customerId,
                    location
                )
        );

        touch(id);
        return new ReserveRoomOperation(
                getTransactionData(id),
                id,
                customerId,
                location)
            .invoke();
    }

    //start
    @Override
    public int start() {
        // database resource managers can't start transactions independently;
        // the middleware does this and then informs the DRM that the
        // transaction has started via the start(int transactionId) methods.
        throw new UnsupportedOperationException();
    }

    @Override
    public synchronized boolean start(int transactionId) {
        if(transactions.get(transactionId) != null)
            return false;

        Trace.info(String.format(
                    "Starting transaction %d.",
                    transactionId));

        transactions.put(
                transactionId,
                new TransactionDataStore(
                    getDatabaseName(),
                    transactionId,
                    lockManager,
                    mainData
                )
        );
        return true;
    }

    //commit
    @Override
    public synchronized boolean commit(int id)
    throws NoSuchTransactionException {
        final TransactionDataStore txData = transactions.get(id);

        Trace.info(String.format(
                    "Entering prepared state for transaction %d.",
                    id));

        if(txData == null)
            throw new NoSuchTransactionException(id);


        preparedTransactions.put(id, transactions.get(id));
        transactions.remove(id);
        try{
            transactionListPersistenceLayer.persist(getTransactionList());
            new SecurePersistenceLayer<Data>(TransactionDataStore.getTransactionFileName(dbname,id))
                .persist(preparedTransactions.get(id).unsafeGetTransactionData());

        } catch(IOException e) {
            UncheckedThrow.throwUnchecked(e);
        }
        lockManager.incrementPreparedTransactionCount();
        return true;
    }

    @Override
    public synchronized boolean partialCommit(int id) {
        throw new UnsupportedOperationException(); 
    }

    @Override
    public synchronized boolean mergeCommit(int id)
    throws NoSuchTransactionException {
        final TransactionDataStore txData = preparedTransactions.get(id);

        Trace.info(String.format(
            "Commiting prepared Transaction %d.",
            id));

        if(txData == null) {
            throw new NoSuchTransactionException(id);
        }
        txData.merge();
        preparedTransactions.remove(id);
        lockManager.releaseTransaction(id);
        lockManager.decrementPreparedTransactionCount();
        try {
            transactionListPersistenceLayer.persist(getTransactionList());
        }
        catch(IOException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
        return true;
    }

    //abort
    @Override
    public synchronized boolean abort(int id)
    throws NoSuchTransactionException {
        final TransactionDataStore txData = transactions.get(id);

        Trace.info(String.format(
                    "Aborting transaction %d.",
                    id));

        if(txData == null)
            throw new NoSuchTransactionException(id);

        transactions.remove(id);
        lockManager.releaseTransaction(id);

        return true;
    }

    @Override
    public boolean shutdown() {
        System.exit(0);
        return true;
    }

    @Override
    public Transaction.State getTransactionStatus(int id) {
        throw new UnsupportedOperationException();
    }

    private class TtlChecker implements Runnable {
        private final int transactionId;
        private long then;

        public TtlChecker(int transactionId) {
            this.transactionId = transactionId;
            this.then = System.currentTimeMillis();
        }

        @Override
        public void run() {
            long now = System.currentTimeMillis();
            long delta = (int)(then - now);

            synchronized(ttls) {
                int ttl = ttls.get(transactionId);
                if(ttl < delta / 1000)
                    try {
                        middleware.abort(transactionId);
                    }
                    catch(NoSuchTransactionException e) {
                    }
                else {
                    ttls.put(transactionId, (int)(ttl - delta / 1000));
                    scheduleTtlCheck(transactionId, ttl * 1000 - delta);
                }
            }
        }
    }
}
