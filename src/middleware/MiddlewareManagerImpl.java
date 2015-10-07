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
    private String customerHost;
    private Integer customerPort; 
    private String rmServiceName;
    private String flightHost;
    private Integer flightPort;
    private String carHost;
    private Integer carPort;
    private String roomHost;
    private Integer roomPort;
    private boolean initialized = false;
    private ResourceManager flightManager, carManager, roomManager, customerManager;

    private void initializeEnv() throws NamingException, MalformedURLException {
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

	//hardocded service name and service hosts could be moved to config file
        rmServiceName = "rm";

        flightHost = "52.88.147.185";
        flightPort = 8080;

        carHost = "54.69.201.163";
        carPort = 8080;

        roomHost = "54.148.36.47";
        roomPort = 8080;
	
	customerHost = "52.89.36.206";
	customerPort=8080; 

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

	URL customerWsdlLocation = new URL(
		"http",
		roomHost,
		roomPort,
		"/" + rmServiceName + "/service?wsdl"
	);
	ResourceManagerImplService customerService = new ResourceManagerImplService(
		customerWsdlLocation 	
	); 
	customerManager = customerService.getResourceManagerImplPort(); 

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
		try {
			initializeEnv(); 
		} catch (Exception e) {
			e.printStackTrace();
		}
		return flightManager.deleteFlight(id,flightNumber); 
	
    }

    /* Return the number of empty seats in this flight. */
    @Override
    public int queryFlight(int id, int flightNumber) {
		try{
			initializeEnv();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return flightManager.queryFlight(id,flightNumber);
    }

    /* Return the price of a seat on this flight. */
    @Override
    public int queryFlightPrice(int id, int flightNumber) {
		try{ 
			initializeEnv();
		} catch(Exception e) {
			e.printStackTrace(); 
		}
		return flightManager.queryFlightPrice(id,flightNumber); 
    }


    // Car operations //

    /* Add cars to a location.  
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
		try{
			initializeEnv();
		} catch(Exception e) {
			e.printStackTrace();
		}
		return carManager.addCars(id,location,numCars,carPrice); 
    }
    
    /* Delete all cars from a location.
     * It should not succeed if there are reservations for this location.
     */		    
    @Override
    public boolean deleteCars(int id, String location) {
		try{
			initializeEnv();
		} catch(Exception e) {
			e.printStackTrace();
		}
		return carManager.deleteCars(id, location);
    }	

    /* Return the number of cars available at this location. */
    @Override
    public int queryCars(int id, String location) {
		try{
			initializeEnv();
		}catch(Exception e) {
			e.printStackTrace(); 
		}
		return carManager.queryCars(id,location); 
    }

    /* Return the price of a car at this location. */
    @Override
    public int queryCarsPrice(int id, String location) {
		try{
			initializeEnv(); 
		}catch(Exception e) {
			e.printStackTrace();
		}
		return carManager.queryCarsPrice(id, location); 
    }


    // Room operations //
    
    /* Add rooms to a location.  
     * This should look a lot like addFlight, only keyed on a string location
     * instead of a flight number.
     */
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice) {
		try{
			initializeEnv(); 
		} catch(Exception e) {
			e.printStackTrace();
		}
		return roomManager.addRooms(id,location, numRooms, roomPrice); 
    }

    /* Delete all rooms from a location.
     * It should not succeed if there are reservations for this location.
     */
    @Override
    public boolean deleteRooms(int id, String location) {
		try{
			initializeEnv();
		} catch(Exception e) {
			e.printStackTrace();
		}
		return roomManager.deleteRooms(id,location); 
    }

    /* Return the number of rooms available at this location. */
    @Override
    public int queryRooms(int id, String location) {
		try{
			initializeEnv();
		} catch(Exception e) {
			e.printStackTrace();
		}
		return roomManager.queryRooms(id,location); 
    }

    /* Return the price of a room at this location. */
    @Override
    public int queryRoomsPrice(int id, String location) {
		try{
			initializeEnv();
		} catch(Exception e) {
			e.printStackTrace();
		}
		return roomManager.queryRoomsPrice(id,location); 
    }


    // Customer operations //
    //NOTE: temporarilly the curstomer info will be passed to all
	//managers however this will later be changed to a database write 
    /* Create a new customer and return their unique identifier. */
    @Override
    public int newCustomer(int id) {
		try{
			initializeEnv();
		} catch(Exception e) {
			e.printStackTrace();
		}
		return customerManager.newCustomer(id); 
    }
    
    /* Create a new customer with the provided identifier. */
    @Override
    public boolean newCustomerId(int id, int customerId){
		try{
			initializeEnv();
		}catch(Exception e) {
			e.printStackTrace();
		}
		return customerManager.newCustomerId(id, customerId);
    }

    /* Remove this customer and all their associated reservations. */
    @Override
    public boolean deleteCustomer(int id, int customerId) {
		try{
			initializeEnv(); 
		}catch(Exception e) {
			e.printStackTrace();
		}
		return customerManager.deleteCustomer(id, customerId); 
    }

    /* Return a bill. */
    @Override
    public String queryCustomerInfo(int id, int customerId) {
		try{
			initializeEnv();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return customerManager.queryCustomerInfo(id,customerId); 
    }

    /* Reserve a seat on this flight. */
    @Override
    public boolean reserveFlight(int id, int customerId, int flightNumber) {
		try{
			initializeEnv();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return flightManager.reserveFlight(id, customerId, flightNumber); 
    }

    /* Reserve a car at this location. */
    @Override
    public boolean reserveCar(int id, int customerId, String location) {
		try{
			initializeEnv();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return carManager.reserveCar(id, customerId, location);
    }

    /* Reserve a room at this location. */
    @Override
    public boolean reserveRoom(int id, int customerId, String location) {
		try {
			initializeEnv();
		} catch (Exception e) {
			e.printStackTrace(); 
		}
		return roomManager.reserveRoom(id,customerId,location);
    }


    /* Reserve an itinerary. */
    @Override
    public boolean reserveItinerary(int id, int customerId, Vector flightNumbers, 
                                    String location, boolean car, boolean room){
	return customerManager.reserveItinerary(id,customerId,flightNumbers,location,car,room); 	
    }
}
