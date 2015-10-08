// -------------------------------
// Adapted from Kevin T. Manley
// CSE 593
// -------------------------------

package server;

import common.*;
import server.ws.*;
import java.beans.PropertyVetoException;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import javax.jws.WebService;
import javax.naming.Context;
import javax.naming.InitialContext;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;
import javax.naming.NamingException;
import java.net.URL;
import java.net.MalformedURLException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import common.UncheckedThrow;
import java.util.*;
import org.apache.commons.dbcp2.BasicDataSource;

@WebService(endpointInterface="server.ws.ResourceManager")
public class ResourceManagerImpl implements ResourceManager {
    public BasicDataSource database;
    boolean initialized = false;
    public void initializeEnv() {
        String dbUrl="";
        String dbUsername="";
        String dbPassword="";
        if(initialized)
                return;

        try{
                File f = new File("sql.secrets");
                if(f.exists() && !f.isDirectory()) {
                    BufferedReader br = new BufferedReader(new FileReader("sql.secrets"));
                  dbUrl=br.readLine();
                   dbUsername=br.readLine();
                        } else {
                                System.out.println("secrets file not found in project root directory");
                        }
                } catch (Exception e) {
                        e.printStackTrace();
                }

        database = new BasicDataSource();
        database.setDriverClassName("org.postgresql.Driver");
        database.setUsername(dbUsername);
        database.setPassword(dbPassword);
        database.setUrl(dbUrl);
        initialized=true;
    }


    // Flight operations //

    // Create a new flight, or add seats to existing flight.
    // Note: if flightPrice <= 0 and the flight already exists, it maintains
    // its current price.
    @Override
    public boolean addFlight(int id, int flightNumber,
                             int numSeats, int flightPrice) {
	try{
		initializeEnv(); 
	}catch(Exception e){e.printStackTrace();}
        Trace.info("RM::addFlight(" + id + ", " + flightNumber
                + ", $" + flightPrice + ", " + numSeats + ") called.");

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            for(int i = 0; i < numSeats; i++) {
                final PreparedStatement stmt = connection.prepareStatement(
                        "INSERT INTO item ( flight_number, price ) " +
                        "VALUES ( ?, ? ) "
                );
                stmt.setInt(1, flightNumber);
                stmt.setInt(2, flightPrice);
                stmt.executeUpdate();
            }

            connection.commit();
            return true;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    @Override
    public boolean deleteFlight(int id, int flightNumber) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::deleteFlight(%d, %d)",
                    id,
                    flightNumber
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "DELETE FROM item " +
                    "WHERE flight_number = ? "
            );
            stmt.setInt(1, flightNumber);
            stmt.executeUpdate();

            // Cascading deletes ensure that reservations are released.

            connection.commit();
            return true;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns the number of empty seats on this flight.
    @Override
    public int queryFlight(int id, int flightNumber) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::queryFlight(%d, %d)",
                    id,
                    flightNumber
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT COUNT(i.id) " +
                    "FROM item i " +
                    "WHERE NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM item_reservation ir " +
                    "        WHERE ir.item_id = i.id " +
                    "      ) " +
                    "  AND i.flight_number = ? "
            );
            stmt.setInt(1, flightNumber);

            final ResultSet rs = stmt.executeQuery();
            rs.next();
            final int count = rs.getInt(1);

            connection.commit();
            return count;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns price of this flight.
    public int queryFlightPrice(int id, int flightNumber) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::queryFlightPrice(%d, %d)",
                    id,
                    flightNumber
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT MIN(i.price) " +
                    "FROM item i " +
                    "WHERE NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM item_reservation ir " +
                    "        WHERE ir.item_id = i.id " +
                    "      ) " +
                    "  AND i.flight_number = ? "
            );
            stmt.setInt(1, flightNumber);

            final ResultSet rs = stmt.executeQuery();
            rs.next();

            final int price = rs.getInt(1);

            if(rs.wasNull()) {
                Trace.warn(
                        String.format(
                            "RM::queryFlightPrice(%d, %s): " +
                            "no flight for minimum price",
                            id,
                            flightNumber
                        )
                );
                return -1; // indicates error to the client
            }

            connection.commit();
            return price;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Car operations //

    // Create a new car location or add cars to an existing location.
    // Note: if price <= 0 and the car location already exists, it maintains
    // its current price.
    @Override
    public boolean addCars(int id, String location, int numCars, int carPrice) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info("RM::addCars(" + id + ", " + location + ", "
                + numCars + ", $" + carPrice + ") called.");
        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            for(int i = 0; i < numCars; i++) {
                final PreparedStatement stmt = connection.prepareStatement(
                        "INSERT INTO item ( location, price ) " +
                        "VALUES ( ?, ? ) "
                );
                stmt.setString(1, location);
                stmt.setInt(2, carPrice);
                stmt.executeUpdate();
            }

            connection.commit();
            return true;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Delete cars from a location.
    @Override
    public boolean deleteCars(int id, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::deleteCars(%d, %s)",
                    id,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "DELETE FROM item AS i " +
                    "WHERE i.location = ? "
            );
            stmt.setString(1, location);
            stmt.executeUpdate();

            connection.commit();
            return true;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns the number of cars available at a location.
    @Override
    public int queryCars(int id, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::queryCars(%d, %s)",
                    id,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT COUNT(1) " +
                    "FROM item i " +
                    "WHERE i.location = ? " +
                    "  AND NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM item_reservation ir " +
                    "        WHERE ir.item_id = i.id " +
                    "      ) "
            );
            stmt.setString(1, location);

            final ResultSet rs = stmt.executeQuery();
            rs.next();
            final int count = rs.getInt(1);

            connection.commit();
            return count;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns price of cars at this location.
    @Override
    public int queryCarsPrice(int id, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::queryCarsPrice(%d, %s)",
                    id,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT MIN(i.price) " +
                    "FROM item i" +
                    "WHERE i.location = ? " + 
                    "  AND NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM item_reservation ir " +
                    "        WHERE ir.item_id = i.id " +
                    "      ) "
            );
            stmt.setString(1, location);

            final ResultSet rs = stmt.executeQuery();
            rs.next();

            final int price = rs.getInt(1);

            if(rs.wasNull()) {
                Trace.warn(
                        String.format(
                            "RM::queryCarsPrice(%d, %s): " +
                            "no cars for minimum price",
                            id,
                            location
                        )
                );
                return -1; // indicates error to the client
            }

            connection.commit();
            return price;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Room operations //

    // Create a new room location or add rooms to an existing location.
    // Note: if price <= 0 and the room location already exists, it maintains
    // its current price.
    @Override
    public boolean addRooms(int id, String location, int numRooms, int roomPrice) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::addRooms(%d, %s, %d, $%d)",
                    id,
                    location,
                    numRooms,
                    roomPrice
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            for(int i = 0; i < numRooms; i++) {
                final PreparedStatement stmt = connection.prepareStatement(
                        "INSERT INTO item ( location, price ) " +
                        "VALUES ( ?, ? ) "
                );
                stmt.setString(1, location);
                stmt.setInt(2, roomPrice);
                stmt.executeUpdate();
            }

            connection.commit();
            return true;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Delete rooms from a location.
    @Override
    public boolean deleteRooms(int id, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::deleteRooms(%d, %s)",
                    id,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "DELETE FROM item AS i " +
                    "WHERE i.location = ? "
            );
            stmt.setString(1, location);
            stmt.executeUpdate();

            connection.commit();
            return true;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns the number of rooms available at a location.
    @Override
    public int queryRooms(int id, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::queryRooms(%d, %s)",
                    id,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT COUNT(1) " +
                    "FROM item i " +
                    "WHERE i.location = ? " +
                    "  AND NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM item_reservation ir " +
                    "        WHERE ir.item_id = i.id " +
                    "      ) "
            );
            stmt.setString(1, location);

            final ResultSet rs = stmt.executeQuery();
            rs.next();
            final int count = rs.getInt(1);

            connection.commit();
            return count;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Returns room price at this location.
    @Override
    public int queryRoomsPrice(int id, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::queryRoomsPrice(%d, %s)",
                    id,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT MIN(i.price) " +
                    "FROM item i " +
                    "WHERE i.location = ? "
            );
            stmt.setString(1, location);

            final ResultSet rs = stmt.executeQuery();
            rs.next();

            final int price = rs.getInt(1);

            if(rs.wasNull()) {
                Trace.warn(
                        String.format(
                            "RM::queryRoomsPrice(%d, %s): " +
                            "no rooms for minimum price",
                            id,
                            location
                        )
                );
                return -1; // indicates error to the client
            }

            connection.commit();
            return price;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }


    // Customer operations //

    @Override
    public int newCustomer(int id) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "INFO: RM::newCustomer(%d)",
                    id
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "INSERT INTO customer " +
                    "DEFAULT VALUES " +
                    "RETURNING id "
            );

            final ResultSet rs = stmt.executeQuery();
            rs.next();

            final int customerId = rs.getInt(1);

            connection.commit();
            return customerId;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }



    @Override
    public boolean newCustomerId(int id, int customerId) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::newCustomerId(%d, %d)",
                    id,
                    customerId
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT COUNT(1) " +
                    "FROM customer c " +
                    "WHERE c.id = ? "
            );
            stmt.setInt(1, customerId);

            final ResultSet rs = stmt.executeQuery();
            rs.next();
            final int count = rs.getInt(1);

            return count == 1;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }

    }

    // Delete customer from the database.
    @Override
    public boolean deleteCustomer(int id, int customerId) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::deleteCustomer(%d, %d)",
                    id,
                    customerId
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "DELETE FROM item AS i " +
                    "USING item_reservation ir " +
                    "WHERE i.id = ir.item_id AND ir.customer_id = ? "
            );
            stmt.setInt(1, customerId);

            final int rowCount = stmt.executeUpdate();

            connection.commit();
            return rowCount > 0;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Return a bill.
    @Override
       public String queryCustomerInfo(int id, int customerId) {
	try{
	initializeEnv(); 
	} catch(Exception e) {e.printStackTrace();}
        Trace.info(
                String.format(
                    "RM::queryCustomerInfo(%d, %d)",
                    id,
                    customerId
                )
        );

        String column = null;

        try(final Connection connection = database.getConnection()) {
            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT COUNT(location) FROM item LIMIT 1"
            );
            final ResultSet rs = stmt.executeQuery();
            rs.next();
            final int count = rs.getInt(1);

            // If no exceptions have been thrown at this point, then the
            // location column exists
            column = "location";
        }
        catch(SQLException e) {
        }

        if(column == null) {
            try(final Connection connection = database.getConnection()) {
                final PreparedStatement stmt = connection.prepareStatement(
                        "SELECT COUNT(flight_number) FROM item LIMIT 1"
                );
                final ResultSet rs = stmt.executeQuery();
                rs.next();
                final int count = rs.getInt(1);

                column = "flight_number";
            }
            catch(SQLException e) {
                throw UncheckedThrow.throwUnchecked(e);
            }
        }


        StringBuffer sb = new StringBuffer();

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement stmt = connection.prepareStatement(
                    "SELECT i.price, i." + column + "::text " +
                    "FROM item i, item_reservation ir " +
                    "WHERE i.id = ir.item_id " +
                    "  AND ir.customer_id = ? "
            );
            stmt.setInt(1, customerId);

            final ResultSet rs = stmt.executeQuery();

            while(rs.next()) {
                final int price = rs.getInt(1);
                final String name = rs.getString(2);
                sb.append(
                        String.format(
                            "%s -- $%d.00\n",
                            name,
                            price
                        )
                );
            }

            connection.commit();
            return sb.toString();
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }
    // Add flight reservation to this customer.
    @Override
    public boolean reserveFlight(int id, int customerId, int flightNumber) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::reserveFlight(%d, %d, %d)",
                    id,
                    customerId,
                    flightNumber
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final int insertedRows = insertFlightReservation(
                    connection,
                    customerId,
                    flightNumber
            ).executeUpdate();

            connection.commit();
            return insertedRows > 0;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Add car reservation to this customer.
    @Override
    public boolean reserveCar(int id, int customerId, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::reserveCar(%d, %d, %s)",
                    id,
                    customerId,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final int insertedRows = insertCarReservation(
                    connection,
                    customerId,
                    location
            ).executeUpdate();

            connection.commit();
            return insertedRows > 0;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    // Add room reservation to this customer.
    @Override
    public boolean reserveRoom(int id, int customerId, String location) {
	  try{
                initializeEnv();
        }catch(Exception e){e.printStackTrace();}

        Trace.info(
                String.format(
                    "RM::reserveRoom(%d, %d, %s)",
                    id,
                    customerId,
                    location
                )
        );

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final int insertedRows = insertRoomReservation(
                    connection,
                    customerId,
                    location
            ).executeUpdate();

            connection.commit();
            return insertedRows > 0;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    private PreparedStatement insertFlightReservation(
            Connection connection,
            int customerId,
            int flightNumber
    ) throws SQLException {
        final PreparedStatement stmt = connection.prepareStatement(
                "INSERT INTO item_reservation " +
                "            ( item_id, customer_id ) " +
                "SELECT i.id, ? " +
                "FROM item i " +
                "WHERE NOT EXISTS ( " +
                "        SELECT 1 " +
                "        FROM item_reservation ir " +
                "        WHERE ir.item_id = i.id " +
                "      ) " +
                "  AND i.flight_number = ? " +
                "ORDER BY i.price ASC " +
                "LIMIT 1 "
        );
        stmt.setInt(1, customerId);
        stmt.setInt(2, flightNumber);

        return stmt;
    }

    private PreparedStatement insertCarReservation(
            Connection connection,
            int customerId,
            String location
    ) throws SQLException {
        final PreparedStatement stmt = connection.prepareStatement(
                "INSERT INTO item_reservation " +
                "            ( item_id, customer_id ) " +
                "SELECT i.id, ? " +
                "FROM item i " +
                "WHERE NOT EXISTS ( " +
                "        SELECT 1 " +
                "        FROM item_reservation ir " +
                "        WHERE ir.item_id = i.id " +
                "      ) " +
                "  AND i.location = ? " +
                "ORDER BY i.price ASC " +
                "LIMIT 1 "
        );
        stmt.setInt(1, customerId);
        stmt.setString(2, location);

        return stmt;
    }

    private PreparedStatement insertRoomReservation(
            Connection connection,
            int customerId,
            String location
    ) throws SQLException {
        final PreparedStatement stmt = connection.prepareStatement(
                "INSERT INTO item_reservation " +
                "            ( item_id, customer_id ) " +
                "SELECT i.id, ? " +
                "FROM item i " +
                "WHERE NOT EXISTS ( " +
                "        SELECT 1 " +
                "        FROM item_reservation ir " +
                "        WHERE ir.item_id = i.id " +
                "      ) " +
                "  AND i.location = ? " +
                "ORDER BY i.price ASC " +
                "LIMIT 1 "
        );
        stmt.setInt(1, customerId);
        stmt.setString(2, location);

        return stmt;
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
        throw new UnsupportedOperationException();
    }
}
