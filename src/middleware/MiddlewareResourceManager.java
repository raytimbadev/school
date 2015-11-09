package middleware;
import transactionmanager.*;
import common.ResourceManager;
import common.UncheckedThrow;
import java.util.*;
import javax.jws.WebService;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.net.URL;
import java.net.MalformedURLException;

public class MiddlewareResourceManager implements ResourceManager {

    final ResourceManager flightManager;
    final ResourceManager carManager;
    final ResourceManager roomManager;
    final ResourceManager customerManager;

    final TransactionManager transactionManager;

    public MiddlewareResourceManager(
            ResourceManager flightManager,
            ResourceManager carManager,
            ResourceManager roomManager,
            ResourceManager customerManager) {
        this.flightManager = flightManager;
        this.carManager = carManager;
        this.roomManager = roomManager;
        this.customerManager = customerManager;

        transactionManager = new TransactionManager();
    }

    // Flight operations //

    /* Add seats to a flight.
     * In general, this will be used to create a new flight, but it should be
     * possible to add seats to an existing flight.  Adding to an existing
     * flight should overwrite the current price of the available seats.
     *
     * @return success.
     */
    @Override
    public boolean addFlight(
            int id,
            int flightNumber,
            int numSeats,
            int flightPrice,
			int tid) {
			if(tid != -1){ //we are part of a transaction
                try {
                    transactionManager.enlist(tid, flightManager);
                }
                catch(NoSuchTransactionException e) {
                    throw UncheckedThrow.throwUnchecked(e);
                }
			}
			return flightManager.addFlight(
                id,
                flightNumber,
                numSeats,
                flightPrice,
				tid);

    }

    /**
     * Delete the entire flight.
     * This implies deletion of this flight and all its seats.  If there is a
     * reservation on the flight, then the flight cannot be deleted.
     *
     * @return success.
     */
    @Override
    public boolean deleteFlight(int id, int flightNumber, int tid) {
		if(tid != -1) { //part of a transaction
            try {
                transactionManager.enlist(tid, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return flightManager.deleteFlight(
                id,
                flightNumber,
				tid);
    }

    /* Return the number of empty seats in this flight. */
    @Override
    public int queryFlight(int id, int flightNumber, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return flightManager.queryFlight(
                id,
                flightNumber,
				tid);
    }

    /* Return the price of a seat on this flight. */
    @Override
    public int queryFlightPrice(int id, int flightNumber, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return flightManager.queryFlightPrice(
                id,
                flightNumber,
				tid);
    }


    // Car operations //

    /* Add cars to a location.
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.addCars(
                id,
                location,
                numCars,
                carPrice,
				tid);
    }

    /* Delete all cars from a location.
     * It should not succeed if there are reservations for this location.
     */
    @Override
    public boolean deleteCars(int id, String location, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.deleteCars(
                id,
                location
				,tid);
    }

    /* Return the number of cars available at this location. */
    @Override
    public int queryCars(int id, String location, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.queryCars(
                id,
                location,
				tid);
    }

    /* Return the price of a car at this location. */
    @Override
    public int queryCarsPrice(int id, String location, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.queryCarsPrice(
                id,
                location,
				tid);
    }

    // Room operations //

    /* Add rooms to a location.
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.addRooms(
                id,
                location,
                numRooms,
                roomPrice,
				tid);
    }

    /* Delete all rooms from a location.
     * It should not succeed if there are reservations for this location.
     */
    @Override
    public boolean deleteRooms(int id, String location, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.deleteRooms(
                id,
                location,
				tid);
    }

    /* Return the number of rooms available at this location. */
    @Override
    public int queryRooms(int id, String location, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.queryRooms(
                id,
                location,
				tid);
    }

    /* Return the price of a room at this location. */
    @Override
    public int queryRoomsPrice(int id, String location, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.queryRoomsPrice(
                id,
                location,
				tid);
    }

    // Customer operations //

    /* Create a new customer and return their unique identifier. */
    @Override
    public int newCustomer(int id, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, customerManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return customerManager.newCustomer(id, tid);
    }

    /* Create a new customer with the provided identifier. */
    @Override
    public boolean newCustomerId(int id, int customerId, int tid){
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, customerManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return customerManager.newCustomerId(id, customerId, tid);
    }

    /* Remove this customer and all their associated reservations. */
    @Override
    public boolean deleteCustomer(int id, int customerId, int tid) {
		if(tid != -1) {
            try {
                transactionManager.enlist(tid, customerManager);
                transactionManager.enlist(tid, flightManager);
                transactionManager.enlist(tid, carManager);
                transactionManager.enlist(tid, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        roomManager.deleteCustomer(id, customerId, tid);
        flightManager.deleteCustomer(id, customerId, tid);
        carManager.deleteCustomer(id, customerId, tid);
        return customerManager.deleteCustomer(id, customerId, tid);
    }

    /* Return a bill. */
    @Override
    public String queryCustomerInfo(int id, int customerId, int tid) {

        final StringBuilder sb = new StringBuilder();
        final boolean exists = customerManager.newCustomerId(id, customerId, tid);
		if( tid != -1) {
            try {
                transactionManager.enlist(tid, customerManager);
                transactionManager.enlist(tid, flightManager);
                transactionManager.enlist(tid, carManager);
                transactionManager.enlist(tid, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        if(exists) {
            sb.append("\nFlights:\n");
            sb.append(flightManager.queryCustomerInfo(id, customerId, tid));

            sb.append("\nRooms:\n");
            sb.append(roomManager.queryCustomerInfo(id, customerId, tid));

            sb.append("\nCars:\n");
            sb.append(carManager.queryCustomerInfo(id, customerId, tid));
        }
        else {
            sb.append(
                    String.format(
                        "\nNo customer with id %d.",
                        customerId
                    )
            );
        }

        return sb.toString();
    }

    /* Reserve a seat on this flight. */
    @Override
    public boolean reserveFlight(int id, int customerId, int flightNumber, int tid) {
        if(tid != -1) {
            try {
                transactionManager.enlist(tid, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }

        if(customerManager.newCustomerId(id, customerId, tid) == false){
            return false;
        }

        return flightManager.reserveFlight(
                id,
                customerId,
                flightNumber,
                tid);
    }

    /* Reserve a car at this location. */
        @Override
    public boolean reserveCar(int id, int customerId, String location, int tid) {
        if(customerManager.newCustomerId(id, customerId, tid) == false)
            return false;

        if(tid != -1) {
            try {
                transactionManager.enlist(tid, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }
            return carManager.reserveCar(
                    id,
                    customerId,
                    location,
                    tid);
    }

    /* Reserve a room at this location. */
    @Override
    public boolean reserveRoom(int id, int customerId, String location, int tid) {
        if(tid != -1) {
            try {
                transactionManager.enlist(tid, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }

        if(customerManager.newCustomerId(id, customerId, tid) == false)
            return false;

        return roomManager.reserveRoom(
                id,
                customerId,
                location,
                tid);
    }


    /* Reserve an itinerary. */
    @Override
    public boolean reserveItinerary(
            int id,
            int customerId,
            Vector flightNumbers,
            String location,
            boolean car,
            boolean room, int tid) {
		if (tid != -1) {
            try {
                transactionManager.enlist(tid, flightManager);
                if(car)
                    transactionManager.enlist(tid, carManager);
                if(room)
                    transactionManager.enlist(tid, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}

        if(customerManager.newCustomerId(id, customerId, tid) == false)
            return false; // customer does not exist

        for(int i=0; i < flightNumbers.size(); i++) {
            final int flightNumber =
                Integer.parseInt((String)flightNumbers.get(i));
            final boolean success =
                flightManager.reserveFlight(id, customerId, flightNumber, tid);
            if(!success)
                return false;
        }

        if(car) {
            final boolean success =
                carManager.reserveCar(id, customerId, location, tid);
            if(!success)
                return false;
        }

        if(room) {
            final boolean success =
                roomManager.reserveCar(id, customerId, location, tid);
            if(!success)
                return false;
        }

        return true;
    }

	/*Start a transaction*/
    @Override
	public int start() {
        final Transaction txn = transactionManager.start();
        return txn.getId();
	}

    @Override
    public boolean start(int transactionId) {
        throw new UnsupportedOperationException();
    }

	/*Commit a transaction with specified id*/
    @Override
	public boolean commit(int id) {
        boolean result = false;

        try {
            result = transactionManager.commit(id);
        }
        catch(NoSuchTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }

        return result;
	}

	/*aborts a transaction witha  specified id*/
    @Override
	public boolean abort(int id)  {
        boolean result = false;

        try {
            result = transactionManager.abort(id);
        }
        catch(NoSuchTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }

        return result;
	}
}

