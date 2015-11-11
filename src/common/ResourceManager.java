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
    public boolean addFlight(int id, int flightNumber, int numSeats, int flightPrice);

    /**
     * Deletes the entire flight.
     *
     * This implies deletion of this flight and all its seats.  If there is a
     * reservation on the flight, then the flight cannot be deleted.
     *
     * @return success.
     */
    public boolean deleteFlight(int id, int flightNumber);

    /**
     * Gets the number of empty seats in this flight.
     */
    public int queryFlight(int id, int flightNumber);

    /**
     * Gets the minimum price of a seat on this flight.
     */
    public int queryFlightPrice(int id, int flightNumber);


    // Car operations //

    /**
     * Adds cars to a location.
     *
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    public boolean addCars(int id, String location, int numCars, int carPrice);

    /**
     * Deletes all cars from a location.
     *
     * It should not succeed if there are reservations for this location.
     */
    public boolean deleteCars(int id, String location);

    /**
     * Gets the number of cars available at this location.
     */
    public int queryCars(int id, String location);

    /**
     * Gets the price of a car at this location.
     */
    public int queryCarsPrice(int id, String location);


    // Room operations //

    /**
     * Adds rooms to a location.
     *
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    public boolean addRooms(int id, String location, int numRooms, int roomPrice);

    /**
     * Deletes all rooms from a location.
     *
     * It should not succeed if there are reservations for this location.
     */
    public boolean deleteRooms(int id, String location);

    /**
     * Gets the number of rooms available at this location.
     */
    public int queryRooms(int id, String location);

    /**
     * Return the price of a room at this location.
     */
    public int queryRoomsPrice(int id, String location);

    // Customer operations //

    /**
     * Create a new customer and return their unique identifier.
     */
    public int newCustomer(int id);

    /**
     * Create a new customer with the provided identifier.
     */
    public boolean newCustomerId(int id, int customerId);

    /**
     * Check if a customer exists.
     */
    public boolean doesCustomerExist(int id, int customerId);

    /**
     * Remove this customer and all their associated reservations.
     */
    public boolean deleteCustomer(int id, int customerId);

    /**
     * Return a bill.
     */
    public String queryCustomerInfo(int id, int customerId);

    /**
     * Reserve a seat on this flight.
     */
    public boolean reserveFlight(int id, int customerId, int flightNumber);

    /**
     * Reserve a car at this location.
     */
    public boolean reserveCar(int id, int customerId, String location);

    /**
     * Reserve a room at this location.
     */
    public boolean reserveRoom(int id, int customerId, String location);


    /**
     * Reserves an itinerary.
     */
    public boolean reserveItinerary(int id, int customerId, Vector
            flightNumbers, String location, boolean car, boolean room);

    /**
     * Starts a new transaction, and returns the identifier for it.
     */
    public int start();

    /**
     * Starts a new transaction with a given identifier.
     *
     * This method will fail if the given transaction identifier is already in
     * use.
     *
     * @param transactionId The identifier with which to create the new
     * transaction.
     * @return Whether the transaction was successfully started or not.
     */
    public boolean start(int transactionId)
        throws RedundantTransactionException;

    /**
     * Commits the transaction associated with the given id.
     */
    public boolean commit(int id)
        throws NoSuchTransactionException;

    /**
     * Abort the transaction associated with id.
     */
    public boolean abort(int id)
        throws NoSuchTransactionException;
}
