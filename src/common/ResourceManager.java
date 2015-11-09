/**
 * Simplified version from CSE 593, University of Washington.
 *
 * A Distributed System in Java using Web Services.
 *
 * Failures should be reported via the return value.  For example,
 * if an operation fails, you should return either false (boolean),
 * or some error code like -1 (int).
 *
 * If there is a boolean return value and you're not sure how it
 * would be used in your implementation, ignore it.  I used boolean
 * return values in the interface generously to allow flexibility in
 * implementation.  But don't forget to return true when the operation
 * has succeeded.
 */

package common;
import java.util.*;

public interface ResourceManager {
    // Flight operations //

    /**
     * Adds seats to a flight.
     *
     * In general, this will be used to create a new flight, but it should be
     * possible to add seats to an existing flight.  Adding to an existing
     * flight should overwrite the current price of the available seats.
     *
     * @return success.
     */
    public boolean addFlight(int id, int flightNumber, int numSeats, int flightPrice, int transaction);

    /**
     * Deletes the entire flight.
     *
     * This implies deletion of this flight and all its seats.  If there is a
     * reservation on the flight, then the flight cannot be deleted.
     *
     * @return success.
     */
    public boolean deleteFlight(int id, int flightNumber, int transaction);

    /**
     * Gets the number of empty seats in this flight.
     */
    public int queryFlight(int id, int flightNumber, int transaction);

    /**
     * Gets the minimum price of a seat on this flight.
     */
    public int queryFlightPrice(int id, int flightNumber, int transaction);


    // Car operations //

    /**
     * Adds cars to a location.
     *
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    public boolean addCars(int id, String location, int numCars, int carPrice, int transaction);

    /**
     * Deletes all cars from a location.
     *
     * It should not succeed if there are reservations for this location.
     */
    public boolean deleteCars(int id, String location, int transaction);

    /**
     * Gets the number of cars available at this location.
     */
    public int queryCars(int id, String location, int transaction);

    /**
     * Gets the price of a car at this location.
     */
    public int queryCarsPrice(int id, String location, int transaction);


    // Room operations //

    /**
     * Adds rooms to a location.
     *
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    public boolean addRooms(int id, String location, int numRooms, int roomPrice, int transaction);

    /**
     * Deletes all rooms from a location.
     *
     * It should not succeed if there are reservations for this location.
     */
    public boolean deleteRooms(int id, String location, int transaction);

    /**
     * Gets the number of rooms available at this location.
     */
    public int queryRooms(int id, String location, int transaction);

    /**
     * Return the price of a room at this location.
     */
    public int queryRoomsPrice(int id, String location, int transaction);

    // Customer operations //

    /**
     * Create a new customer and return their unique identifier.
     */
    public int newCustomer(int id, int transaction);

    /**
     * Create a new customer with the provided identifier.
     */
    public boolean newCustomerId(int id, int customerId, int transaction);

    /**
     * Remove this customer and all their associated reservations.
     */
    public boolean deleteCustomer(int id, int customerId, int transaction);

    /**
     * Return a bill.
     */
    public String queryCustomerInfo(int id, int customerId, int transaction);

    /**
     * Reserve a seat on this flight.
     */
    public boolean reserveFlight(int id, int customerId, int flightNumber,
            int transaction);

    /**
     * Reserve a car at this location.
     */
    public boolean reserveCar(int id, int customerId, String location,
            int transaction);

    /**
     * Reserve a room at this location.
     */
    public boolean reserveRoom(int id, int customerId, String location,
            int transaction);


    /**
     * Reserves an itinerary.
     */
    public boolean reserveItinerary(int id, int customerId, Vector
            flightNumbers, String location, boolean car, boolean room,
            int transaction);

    /**
     * Starts a transaction.
     */
    public int start();

    /**
     * Commits the transaction associated with the given id.
     */
    public boolean commit(int id);

    /**
     * Abort the transaction associated with id.
     */
    public boolean abort(int id);
}
