package server;

import lockmanager.*;
import common.*;
import common.operations.*;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;
import org.apache.commons.dbcp2.BasicDataSource;

public abstract class DatabaseResourceManager implements ResourceManager {
    protected final LockManager lockManager;
    protected final BasicDataSource database;

    protected final Hashtable<Integer, List<Operation<Object>>> transactions;

    public DatabaseResourceManager(
            String dbUsername,
            String dbPassword,
            String dbUrl
    ) throws IOException, SQLException, PropertyVetoException {
        lockManager = new LockManager();

        database = new BasicDataSource();
        database.setDriverClassName("org.postgresql.Driver");
        database.setUsername(dbUsername);
        database.setPassword(dbPassword);
        database.setUrl(dbUrl);

        transactions = new Hashtable<Integer, List<Operation<Object>>>();
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
		AddFlightOperation op = new AddFlightOperation(id,flightNumber,numSeats,flightPrice); 	
		if(id == -1) {
			return op.invoke(database); 
		}

		//TODO add to list
		return true;
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
		DeleteFlightOperation op = new DeleteFlightOperation(id,flightNumber);
		if(id == -1) {
			return op.invoke(database);
		}
		
		//TODO add to list
		return true; 

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
		QueryFlightOperation op = new QueryFlightOperation(id,flightNumber); 
		if(id == -1) {
			return op.invoke(database); 
		}
		
		//TODO add to list
		return 0; 
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
		QueryFlightPriceOperation op = new QueryFlightPriceOperation(id,flightNumber); 
		if(id == -1) {
			return op.invoke(database);
		}
		//TODO add to list
		return 0; 
    }

    // Car operations //

    // Create a new car location or add cars to an existing location.
    // Note: if price <= 0 and the car location already exists, it maintains
    // its current price.
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
        Trace.info("RM::addCars(" + id + ", " + location + ", "
                + numCars + ", $" + carPrice + ") called.");
		AddCarsOperation op = new AddCarsOperation(id,location,numCars,carPrice); 
		if(id == -1) {
		 return op.invoke(database); 
		}
		
		//TODO add to list
		return true; 
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
		DeleteCarsOperation op = new DeleteCarsOperation(id,location); 
		if(id == -1) {
			op.invoke(database); 
		}
		
		//TODO add to list
		return true; 
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
		QueryCarsOperation op = new QueryCarsOperation(id,location); 
		if(id == -1) {
			op.invoke(database); 
		}
		//TODO add to list
		return 0; 
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

		QueryCarsPriceOperation op = new QueryCarsPriceOperation(id,location); 
		if(id == 01) {
			op.invoke(database);
		}
		//TODO add to list
		return 0; 
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
		AddRoomsOperation op = new AddRoomsOperation(id,location,numRooms,roomPrice); 
		if(id == -1) {
			op.invoke(database);
		}
		//TODO add to list
		return true; 

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
		DeleteRoomsOperation op = new DeleteRoomsOperation(id,location); 
		if(id == -1) {
			op.invoke(database); 
		}
		//TODO add to list
		return true; 
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
		QueryRoomOperation op = new QueryRoomOperation(id,location); 
		if(id == -1) {
            op.invoke(database); 
		}
        //TODO add to list
        return 0; 
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
		QueryRoomPriceOperation op = new QueryRoomPriceOperation(id,location); 
		if(id == -1) {
            return op.invoke(database);
        }
        //TODO add to list
        return 0; 
    }

    @Override
    public int newCustomer(int id) {
        Trace.info(
                String.format(
                    "INFO: RM::newCustomer(%d)",
                    id
                )
        );
	    NewCustomerOperation op = new NewCustomerOperation(id); 
		if( id != -1) {
			throw new UnsupportedOperationException(); 
		}
		return op.invoke(database); 

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
		NewCustomerIdOperation op = new NewCustomerIdOperation(id,customerId);
		if(id != -1) {
            throw new UnsupportedOperationException();
         }
         return op.invoke(database); 
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
        ReserveFlightOperation op = new ReserveFlightOperation(id,customerId,flightNumber); 
		if(id == -1) {
			return op.invoke(database); 
		}
		//TODO add to list
		return true; 
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
		ReserveCarOperation op = new ReserveCarOperation(id,customerId,location); 
	    if(id == -1) {
		    return op.invoke(database); 
	    }
        //TODO add to list
        return true; 

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
        ReserveRoomOperation op = new ReserveRoomOperation(id,customerId,location);
		if(id == -1) {
            return op.invoke(database);
        }
        //TODO add to list 
        return true; 
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
    public synchronized boolean start(int transactionId)
    throws RedundantTransactionException {
        if(transactions.get(transactionId) != null)
            throw new RedundantTransactionException(transactionId);

        transactions.put(transactionId, new ArrayList<Operation<Object>>());
        throw new UnsupportedOperationException();
    }

    //commit
    @Override
    public synchronized boolean commit(int id)
    throws NoSuchTransactionException {
        final List<Operation<Object>> ops = transactions.get(id);

        if(ops == null)
            throw new NoSuchTransactionException(id);

        for(final Operation<Object> op : ops)
            op.invoke(database);

        transactions.remove(id);

        // TODO release locks

        return true;
    }

    //abort
    @Override
    public synchronized boolean abort(int id)
    throws NoSuchTransactionException {
        final List<Operation<Object>> ops = transactions.get(id);

        if(ops == null)
            throw new NoSuchTransactionException(id);

        transactions.remove(id);

        // TODO release locks

        return true;
    }
}
