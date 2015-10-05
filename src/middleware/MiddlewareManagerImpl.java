package middleware;

import java.util.*;
import javax.jws.WebService;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.net.URL;
import java.net.MalformedURLException;

@WebService(endpointInterface = "server.ws.ResourceManager")
public class MiddlewareManagerImpl implements server.ws.ResourceManager {
    private static String rmServiceName;
    private static String flightHost;
    private static Integer flightPort;
    private static String carHost;
    private static Integer carPort;
    private static String roomHost;
    private static Integer roomPort;
    private static boolean initialized = false;

    private static ResourceManager flightManager, carManager, roomManager;

    private static void initializeEnv() throws NamingException, MalformedURLException {
        if (initialized)
            return;

        //System.setProperty(
        //        Context.INITIAL_CONTEXT_FACTORY,
        //        "org.apache.naming.java.javaURLContextFactory"
        //);

        //Context env = (Context) new InitialContext();

        //rmServiceName = (String) env.lookup("service-name");

        //flightHost = (String) env.lookup("flight-service-host");
        //flightPort = (Integer) env.lookup("flight-service-port");

        //carHost = (String) env.lookup("car-service-host");
        //carPort = (Integer) env.lookup("car-service-port");

        //roomHost = (String) env.lookup("room-service-host");
        //roomPort = (Integer) env.lookup("room-service-port");

        // // hardcode this for now

        rmServiceName = "rm";

        flightHost = "localhost";
        flightPort = 8081;

        carHost = "localhost";
        carPort = 8082;

        roomHost = "localhost";
        roomPort = 8083;

        URL flightWsdlLocation = new URL(
                "http",
                flightHost,
                flightPort, 
                "/" + rmServiceName + "/service?wsdl"
        );
                
        URL carWsdlLocation = new URL(
                "http",
                carHost,
                carPort,
                "/" + rmServiceName + "/service?wsdl"
        );

        URL roomWsdlLocation = new URL(
                "http",
                roomHost,
                roomPort,
                "/" + rmServiceName + "/service?wsdl"
        );

        ResourceManagerImplService flightService = new ResourceManagerImplService(
                flightWsdlLocation
        );

        flightManager = flightService.getResourceManagerImplPort();

        ResourceManagerImplService carService = new ResourceManagerImplService(
                carWsdlLocation
        );

        carManager = carService.getResourceManagerImplPort();

        ResourceManagerImplService roomService = new ResourceManagerImplService(
                roomWsdlLocation
        );

        roomManager = roomService.getResourceManagerImplPort();
        initialized = true;
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
    public boolean addFlight(int id, int flightNumber, int numSeats, int flightPrice) {
        try {
            initializeEnv();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return flightManager.addFlight(id, flightNumber, numSeats, flightPrice);
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
