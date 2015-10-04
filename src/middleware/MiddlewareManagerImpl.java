package middleware;

import java.util.*;
import javax.jws.WebService;

@WebService(endpointInterface = "server.ws.ResourceManager")
public class MiddlewareManagerImpl implements server.ws.ResourceManager {
    // Flight operations //
    
    /* Add seats to a flight.  
     * In general, this will be used to create a new flight, but it should be 
     * possible to add seats to an existing flight.  Adding to an existing 
     * flight should overwrite the current price of the available seats.
     *
     * @return success.
     */
    @Override
    public boolean addFlight(int id, int flightNumber, int numSeats, int flightPrice) {
        System.out.println("hi");
        throw new UnsupportedOperationException();
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
        throw new UnsupportedOperationException();
    }

    /* Return the number of empty seats in this flight. */
    @Override
    public int queryFlight(int id, int flightNumber) {
        throw new UnsupportedOperationException();
    }

    /* Return the price of a seat on this flight. */
    @Override
    public int queryFlightPrice(int id, int flightNumber) {
        throw new UnsupportedOperationException();
    }


    // Car operations //

    /* Add cars to a location.  
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
        throw new UnsupportedOperationException();
    }
    
    /* Delete all cars from a location.
     * It should not succeed if there are reservations for this location.
     */		    
    @Override
    public boolean deleteCars(int id, String location) {
        throw new UnsupportedOperationException();
    }

    /* Return the number of cars available at this location. */
    @Override
    public int queryCars(int id, String location) {
        throw new UnsupportedOperationException();
    }

    /* Return the price of a car at this location. */
    @Override
    public int queryCarsPrice(int id, String location) {
        throw new UnsupportedOperationException();
    }


    // Room operations //
    
    /* Add rooms to a location.  
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice) {
        throw new UnsupportedOperationException();
    }

    /* Delete all rooms from a location.
     * It should not succeed if there are reservations for this location.
     */
    @Override
    public boolean deleteRooms(int id, String location) {
        throw new UnsupportedOperationException();
    }

    /* Return the number of rooms available at this location. */
    @Override
    public int queryRooms(int id, String location) {
        throw new UnsupportedOperationException();
    }

    /* Return the price of a room at this location. */
    @Override
    public int queryRoomsPrice(int id, String location) {
        throw new UnsupportedOperationException();
    }


    // Customer operations //
        
    /* Create a new customer and return their unique identifier. */
    @Override
    public int newCustomer(int id) {
        throw new UnsupportedOperationException();
    }
    
    /* Create a new customer with the provided identifier. */
    @Override
    public boolean newCustomerId(int id, int customerId){
        throw new UnsupportedOperationException();
    }

    /* Remove this customer and all their associated reservations. */
    @Override
    public boolean deleteCustomer(int id, int customerId) {
        throw new UnsupportedOperationException();
    }

    /* Return a bill. */
    @Override
    public String queryCustomerInfo(int id, int customerId) {
        throw new UnsupportedOperationException();
    }

    /* Reserve a seat on this flight. */
    @Override
    public boolean reserveFlight(int id, int customerId, int flightNumber) {
        throw new UnsupportedOperationException();
    }

    /* Reserve a car at this location. */
    @Override
    public boolean reserveCar(int id, int customerId, String location) {
        throw new UnsupportedOperationException();
    }

    /* Reserve a room at this location. */
    @Override
    public boolean reserveRoom(int id, int customerId, String location) {
        throw new UnsupportedOperationException();
    }


    /* Reserve an itinerary. */
    @Override
    public boolean reserveItinerary(int id, int customerId, Vector flightNumbers, 
                                    String location, boolean car, boolean room){
        throw new UnsupportedOperationException();
    }
}
