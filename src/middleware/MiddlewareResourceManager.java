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
            int flightPrice) {
			if(id != -1){ //we are part of a transaction
                try {
                    transactionManager.enlist(id, flightManager);
                }
                catch(NoSuchTransactionException e) {
                    throw UncheckedThrow.throwUnchecked(e);
                }
			}
			return flightManager.addFlight(
                id,
                flightNumber,
                numSeats,
                flightPrice);

    }

    /**
     * Delete the entire flight.
     * This implies deletion of this flight and all its seats.  If there is a
     * reservation on the flight, then the flight cannot be deleted.
     *
     * @return success.
     */
    @Override
    public boolean deleteFlight(int id, int flightNumber) {
		if(id != -1) { //part of a transaction
            try {
                transactionManager.enlist(id, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return flightManager.deleteFlight(
                id,
                flightNumber);
    }

    /* Return the number of empty seats in this flight. */
    @Override
    public int queryFlight(int id, int flightNumber) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return flightManager.queryFlight(
                id,
                flightNumber);
    }

    /* Return the price of a seat on this flight. */
    @Override
    public int queryFlightPrice(int id, int flightNumber) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return flightManager.queryFlightPrice(
                id,
                flightNumber);
    }


    // Car operations //

    /* Add cars to a location.
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.addCars(
                id,
                location,
                numCars,
                carPrice);
    }

    /* Delete all cars from a location.
     * It should not succeed if there are reservations for this location.
     */
    @Override
    public boolean deleteCars(int id, String location) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.deleteCars(
                id,
                location);
    }

    /* Return the number of cars available at this location. */
    @Override
    public int queryCars(int id, String location) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.queryCars(
                id,
                location);
    }

    /* Return the price of a car at this location. */
    @Override
    public int queryCarsPrice(int id, String location) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return carManager.queryCarsPrice(
                id,
                location);
    }

    // Room operations //

    /* Add rooms to a location.
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.addRooms(
                id,
                location,
                numRooms,
                roomPrice);
    }

    /* Delete all rooms from a location.
     * It should not succeed if there are reservations for this location.
     */
    @Override
    public boolean deleteRooms(int id, String location) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.deleteRooms(
                id,
                location);
    }

    /* Return the number of rooms available at this location. */
    @Override
    public int queryRooms(int id, String location) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.queryRooms(
                id,
                location);
    }

    /* Return the price of a room at this location. */
    @Override
    public int queryRoomsPrice(int id, String location) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return roomManager.queryRoomsPrice(
                id,
                location);
    }

    // Customer operations //

    /* Create a new customer and return their unique identifier. */
    @Override
    public int newCustomer(int id) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, customerManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return customerManager.newCustomer(id);
    }

    /* Create a new customer with the provided identifier. */
    @Override
    public boolean newCustomerId(int id, int customerId){
		if(id != -1) {
            try {
                transactionManager.enlist(id, customerManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        return customerManager.newCustomerId(id, customerId);
    }

    /* Remove this customer and all their associated reservations. */
    @Override
    public boolean deleteCustomer(int id, int customerId) {
		if(id != -1) {
            try {
                transactionManager.enlist(id, customerManager);
                transactionManager.enlist(id, flightManager);
                transactionManager.enlist(id, carManager);
                transactionManager.enlist(id, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        roomManager.deleteCustomer(id, customerId);
        flightManager.deleteCustomer(id, customerId);
        carManager.deleteCustomer(id, customerId);
        return customerManager.deleteCustomer(id, customerId);
    }

    /* Return a bill. */
    @Override
    public String queryCustomerInfo(int id, int customerId) {

        final StringBuilder sb = new StringBuilder();
        final boolean exists = customerManager.newCustomerId(id, customerId);
		if(id != -1) {
            try {
                transactionManager.enlist(id, customerManager);
                transactionManager.enlist(id, flightManager);
                transactionManager.enlist(id, carManager);
                transactionManager.enlist(id, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}
        if(exists) {
            sb.append("\nFlights:\n");
            sb.append(flightManager.queryCustomerInfo(id, customerId));

            sb.append("\nRooms:\n");
            sb.append(roomManager.queryCustomerInfo(id, customerId));

            sb.append("\nCars:\n");
            sb.append(carManager.queryCustomerInfo(id, customerId));
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
    public boolean reserveFlight(int id, int customerId, int flightNumber) {
        if(id != -1) {
            try {
                transactionManager.enlist(id, flightManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }

        if(customerManager.newCustomerId(id, customerId) == false){
            return false;
        }

        return flightManager.reserveFlight(
                id,
                customerId,
                flightNumber);
    }

    /* Reserve a car at this location. */
        @Override
    public boolean reserveCar(int id, int customerId, String location) {
        if(customerManager.newCustomerId(id, customerId) == false)
            return false;

        if(id != -1) {
            try {
                transactionManager.enlist(id, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }
            return carManager.reserveCar(
                    id,
                    customerId,
                    location);
    }

    /* Reserve a room at this location. */
    @Override
    public boolean reserveRoom(int id, int customerId, String location) {
        if(id != -1) {
            try {
                transactionManager.enlist(id, carManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }

        if(customerManager.newCustomerId(id, customerId) == false)
            return false;

        return roomManager.reserveRoom(
                id,
                customerId,
                location);
    }


    /* Reserve an itinerary. */
    @Override
    public boolean reserveItinerary(
            int id,
            int customerId,
            Vector flightNumbers,
            String location,
            boolean car,
            boolean room) {
		if (id != -1) {
            try {
                transactionManager.enlist(id, flightManager);
                if(car)
                    transactionManager.enlist(id, carManager);
                if(room)
                    transactionManager.enlist(id, roomManager);
            }
            catch(NoSuchTransactionException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
		}

        if(customerManager.newCustomerId(id, customerId) == false)
            return false; // customer does not exist

        for(int i=0; i < flightNumbers.size(); i++) {
            final int flightNumber =
                Integer.parseInt((String)flightNumbers.get(i));
            final boolean success =
                flightManager.reserveFlight(id, customerId, flightNumber);
            if(!success)
                return false;
        }

        if(car) {
            final boolean success =
                carManager.reserveCar(id, customerId, location);
            if(!success)
                return false;
        }

        if(room) {
            final boolean success =
                roomManager.reserveCar(id, customerId, location);
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

