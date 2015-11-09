package client;

import common.ResourceManager;
import common.UncheckedThrow;
import common.sockets.Response;
import common.sockets.Request;
import common.sockets.NetworkCaller;

import java.net.InetAddress;
import java.net.Socket;
import java.util.Vector;

public class SocketResourceManager implements ResourceManager {
    final NetworkCaller network;

    public SocketResourceManager(InetAddress address, int port) {
        network = new NetworkCaller(address, port);
    }

    // Flight operations //

    // Create a new flight, or add seats to existing flight.
    // Note: if flightPrice <= 0 and the flight already exists, it maintains
    // its current price.
    @Override
    public boolean addFlight(
            int id,
            int flightNumber,
            int numSeats,
            int flightPrice) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("addFlight")
                        .primitive(id)
                        .primitive(flightNumber)
                        .primitive(numSeats)
                        .primitive(flightPrice)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    @Override
    public boolean deleteFlight(int id, int flightNumber) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("deleteFlight")
                        .primitive(id)
                        .primitive(flightNumber)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns the number of empty seats on this flight.
    @Override
    public int queryFlight(int id, int flightNumber) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("queryFlight")
                        .primitive(id)
                        .primitive(flightNumber)
                        .build()
            );

            if(response.isSuccessful())
                return (Integer)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns price of this flight.
    public int queryFlightPrice(int id, int flightNumber) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("queryFlightPrice")
                        .primitive(id)
                        .primitive(flightNumber)
                        .build()
            );

            if(response.isSuccessful())
                return (Integer)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Car operations //

    // Create a new car location or add cars to an existing location.
    // Note: if price <= 0 and the car location already exists, it maintains
    // its current price.
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("addCars")
                        .primitive(id)
                        .parameter(location)
                        .primitive(numCars)
                        .primitive(carPrice)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Delete cars from a location.
    @Override
    public boolean deleteCars(int id, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("deleteCars")
                        .primitive(id)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns the number of cars available at a location.
    @Override
    public int queryCars(int id, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("queryCars")
                        .primitive(id)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Integer)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns price of cars at this location.
    @Override
    public int queryCarsPrice(int id, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("queryCarsPrice")
                        .primitive(id)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Integer)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }


    // Room operations //

    // Create a new room location or add rooms to an existing location.
    // Note: if price <= 0 and the room location already exists, it maintains
    // its current price.
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("addRooms")
                        .primitive(id)
                        .parameter(location)
                        .primitive(numRooms)
                        .primitive(roomPrice)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Delete rooms from a location.
    @Override
    public boolean deleteRooms(int id, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("deleteRooms")
                        .primitive(id)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns the number of rooms available at a location.
    @Override
    public int queryRooms(int id, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("queryRooms")
                        .primitive(id)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Integer)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns room price at this location.
    @Override
    public int queryRoomsPrice(int id, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("queryRoomsPrice")
                        .primitive(id)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Integer)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Customer operations //

    @Override
    public int newCustomer(int id) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("newCustomer")
                        .primitive(id)
                        .build()
            );

            if(response.isSuccessful())
                return (Integer)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // This method makes testing easier.
    @Override
    public boolean newCustomerId(int id, int customerId) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("newCustomerId")
                        .primitive(id)
                        .primitive(customerId)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Delete customer from the database.
    @Override
    public boolean deleteCustomer(int id, int customerId) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("deleteCustomer")
                        .primitive(id)
                        .primitive(customerId)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Return a bill.
    @Override
    public String queryCustomerInfo(int id, int customerId) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("queryCustomerInfo")
                        .primitive(id)
                        .primitive(customerId)
                        .build()
            );

            if(response.isSuccessful())
                return (String)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Add flight reservation to this customer.
    @Override
    public boolean reserveFlight(int id, int customerId, int flightNumber) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("reserveFlight")
                        .primitive(id)
                        .primitive(customerId)
                        .primitive(flightNumber)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Add car reservation to this customer.
    @Override
    public boolean reserveCar(int id, int customerId, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("reserveCar")
                        .primitive(id)
                        .primitive(customerId)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Add room reservation to this customer.
    @Override
    public boolean reserveRoom(int id, int customerId, String location) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("reserveRoom")
                        .primitive(id)
                        .primitive(customerId)
                        .parameter(location)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Reserve an itinerary.
    @Override
    public boolean reserveItinerary(
            int id,
            int customerId,
            Vector flightNumbers,
            String location,
            boolean car,
            boolean room) {
        try {
            final Response response = network.invoke(
                    new Request.RequestBuilder()
                        .withMethod("reserveItinerary")
                        .primitive(id)
                        .primitive(customerId)
                        .parameter(flightNumbers)
                        .parameter(location)
                        .primitive(car)
                        .primitive(room)
                        .build()
            );

            if(response.isSuccessful())
                return (Boolean)response.getResult();
            else
                throw UncheckedThrow.throwUnchecked(response.getError());
        }
        catch(Exception e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }
		
	//start
	@Override
	public int start() {
		throw UncheckedThrow.throwUnchecked(new UnsupportedOperationException());
	}
	
	//commit
	public boolean commit(int id)  {
		throw UncheckedThrow.throwUnchecked(new UnsupportedOperationException()); 
	}

	//abort
	public boolean abort(int id)  {
		throw UncheckedThrow.throwUnchecked(new UnsupportedOperationException());
	}
}
