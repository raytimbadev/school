package client;

import common.ResourceManager;
import common.UncheckedThrow;
import common.sockets.Response;
import common.sockets.Request;
import common.sockets.NetworkCaller;
import common.TransactionStatus;

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
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("addFlight")
                    .primitive(id)
                    .primitive(flightNumber)
                    .primitive(numSeats)
                    .primitive(flightPrice)
                    .build()).yield();
    }

    @Override
    public boolean deleteFlight(int id, int flightNumber) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                .withMethod("deleteFlight")
                .primitive(id)
                .primitive(flightNumber)
                .build()).yield();
    }

    // Returns the number of empty seats on this flight.
    @Override
    public int queryFlight(int id, int flightNumber) {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("queryFlight")
                    .primitive(id)
                    .primitive(flightNumber)
                    .build()).yield();
    }

    // Returns price of this flight.
    public int queryFlightPrice(int id, int flightNumber) {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("queryFlightPrice")
                    .primitive(id)
                    .primitive(flightNumber)
                    .build()).yield();
    }

    // Car operations //

    // Create a new car location or add cars to an existing location.
    // Note: if price <= 0 and the car location already exists, it maintains
    // its current price.
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("addCars")
                    .primitive(id)
                    .parameter(location)
                    .primitive(numCars)
                    .primitive(carPrice)
                    .build()).yield();
    }

    // Delete cars from a location.
    @Override
    public boolean deleteCars(int id, String location) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("deleteCars")
                    .primitive(id)
                    .parameter(location)
                    .build()).yield();
    }

    // Returns the number of cars available at a location.
    @Override
    public int queryCars(int id, String location) {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("queryCars")
                    .primitive(id)
                    .parameter(location)
                    .build()).yield();
    }

    // Returns price of cars at this location.
    @Override
    public int queryCarsPrice(int id, String location) {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("queryCarsPrice")
                    .primitive(id)
                    .parameter(location)
                    .build()).yield();
    }


    // Room operations //

    // Create a new room location or add rooms to an existing location.
    // Note: if price <= 0 and the room location already exists, it maintains
    // its current price.
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("addRooms")
                    .primitive(id)
                    .parameter(location)
                    .primitive(numRooms)
                    .primitive(roomPrice)
                    .build()).yield();
    }

    // Delete rooms from a location.
    @Override
    public boolean deleteRooms(int id, String location) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("deleteRooms")
                    .primitive(id)
                    .parameter(location)
                    .build()).yield();
    }

    // Returns the number of rooms available at a location.
    @Override
    public int queryRooms(int id, String location) {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("queryRooms")
                    .primitive(id)
                    .parameter(location)
                    .build()).yield();
    }

    // Returns room price at this location.
    @Override
    public int queryRoomsPrice(int id, String location) {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("queryRoomsPrice")
                    .primitive(id)
                    .parameter(location)
                    .build()).yield();
    }

    // Customer operations //

    @Override
    public int newCustomer(int id) {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("newCustomer")
                    .primitive(id)
                    .build()).yield();
    }

    // This method makes testing easier.
    @Override
    public boolean newCustomerId(int id, int customerId) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("newCustomerId")
                    .primitive(id)
                    .primitive(customerId)
                    .build()).yield();
    }

    @Override
    public boolean doesCustomerExist(int id, int customerId) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("doesCustomerExist")
                    .primitive(id)
                    .primitive(customerId)
                    .build()).yield();
    }

    // Delete customer from the database.
    @Override
    public boolean deleteCustomer(int id, int customerId) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("deleteCustomer")
                    .primitive(id)
                    .primitive(customerId)
                    .build()).yield();
    }

    // Return a bill.
    @Override
    public String queryCustomerInfo(int id, int customerId) {
        return (String)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("queryCustomerInfo")
                    .primitive(id)
                    .primitive(customerId)
                    .build()).yield();
    }

    // Add flight reservation to this customer.
    @Override
    public boolean reserveFlight(int id, int customerId, int flightNumber) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("reserveFlight")
                    .primitive(id)
                    .primitive(customerId)
                    .primitive(flightNumber)
                    .build()).yield();
    }

    // Add car reservation to this customer.
    @Override
    public boolean reserveCar(int id, int customerId, String location) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("reserveCar")
                    .primitive(id)
                    .primitive(customerId)
                    .parameter(location)
                    .build()).yield();
    }

    // Add room reservation to this customer.
    @Override
    public boolean reserveRoom(int id, int customerId, String location) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("reserveRoom")
                    .primitive(id)
                    .primitive(customerId)
                    .parameter(location)
                    .build()).yield();
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
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("reserveItinerary")
                    .primitive(id)
                    .primitive(customerId)
                    .parameter(flightNumbers)
                    .parameter(location)
                    .primitive(car)
                    .primitive(room)
                    .build()).yield();
            }
		
	//start
	@Override
	public int start() {
        return (Integer)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("start")
                    .build()).yield();
    }

    @Override
    public boolean start(int id) {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("start")
                    .primitive(id)
                    .build()).yield();
    }
	
	//commit
    @Override
	public boolean commit(int id)  {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("commit") .primitive(id)
                    .build()).yield();
    }

    //merge commit
    @Override
    public boolean mergeCommit(int id)  {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("mergeCommit") .primitive(id)
                    .build()).yield();
    }

	//abort
    @Override
	public boolean abort(int id)  {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("abort")
                    .primitive(id)
                    .build()).yield();
    }

    @Override
    public boolean shutdown() {
        return (Boolean)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("shutdown")
                    .build()).yield();
    }

    @Override
    public TransactionStatus getTransactionStatus(int id) {
        return (TransactionStatus)network.invoke(
                new Request.RequestBuilder()
                    .withMethod("getTransactionStatus")
                    .build()).yield();
    }
}
