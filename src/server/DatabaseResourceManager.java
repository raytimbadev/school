package server;

import lockmanager.*;
import common.*;
import common.operations.*;

import java.util.*;

public abstract class DatabaseResourceManager implements ResourceManager {
    private static int nextCustomerId = 0;

    private static synchronized int getNextCustomerId() {
        return nextCustomerId++;
    }

    protected final Hashtable<String, ItemGroup> mainData;
    protected final LockManager lockManager;
    protected final TransactionDataStore mainDataStore;
    protected final Hashtable<Integer, TransactionDataStore> transactions;

    protected synchronized void mergeData(Hashtable<String, ItemGroup> data) {
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

    public DatabaseResourceManager() {
        lockManager = new LockManager();
        transactions = new Hashtable<Integer, TransactionDataStore>();
        mainData = new Hashtable<String, ItemGroup>();
        mainDataStore = new TransactionDataStore(
                TransactionOperation.NO_TRANSACTION,
                null,
                mainData);
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

        transactions.put(transactionId, new TransactionDataStore(transactionId,lockManager,mainData));
        return true;
    }

    //commit
    @Override
    public synchronized boolean commit(int id)
    throws NoSuchTransactionException {
        final TransactionDataStore txData = transactions.get(id);

        Trace.info(String.format(
                    "Committing transaction %d.",
                    id));

        if(txData == null)
            throw new NoSuchTransactionException(id);

        txData.merge(); 
        transactions.remove(id);
        lockManager.releaseTransaction(id);
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
}
