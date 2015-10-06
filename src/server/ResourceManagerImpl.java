// -------------------------------
// Adapted from Kevin T. Manley
// CSE 593
// -------------------------------

package server;

import common.*;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;
import org.apache.commons.dbcp2.BasicDataSource;

public class ResourceManagerImpl implements ResourceManager {
    final BasicDataSource database;

    public ResourceManagerImpl(
            String dbUsername,
            String dbPassword,
            String dbUrl
    ) throws IOException, SQLException, PropertyVetoException {
        database = new BasicDataSource();
        database.setDriverClassName("org.postgresql.Driver");
        database.setUsername(dbUsername);
        database.setPassword(dbPassword);
        database.setUrl(dbUrl);
    }

    protected RMHashtable m_itemHT;

    // Basic operations on RMItem //

    // Read a data item.
    private RMItem readData(int id, String key) {
        synchronized(m_itemHT) {
            return (RMItem) m_itemHT.get(key);
        }
    }

    // Write a data item.
    private void writeData(int id, String key, RMItem value) {
        synchronized(m_itemHT) {
            m_itemHT.put(key, value);
        }
    }

    // Remove the item out of storage.
    protected RMItem removeData(int id, String key) {
        synchronized(m_itemHT) {
            return (RMItem) m_itemHT.remove(key);
        }
    }


    // Basic operations on ReservableItem //

    // Delete the entire item.
    protected boolean deleteItem(int id, String key) {
        Trace.info("RM::deleteItem(" + id + ", " + key + ") called.");
        ReservableItem curObj = (ReservableItem) readData(id, key);
        // Check if there is such an item in the storage.
        if (curObj == null) {
            Trace.warn("RM::deleteItem(" + id + ", " + key + ") failed: "
                    + " item doesn't exist.");
            return false;
        } else {
            if (curObj.getReserved() == 0) {
                removeData(id, curObj.getKey());
                Trace.info("RM::deleteItem(" + id + ", " + key + ") OK.");
                return true;
            }
            else {
                Trace.info("RM::deleteItem(" + id + ", " + key + ") failed: "
                        + "some customers have reserved it.");
                return false;
            }
        }
    }

    // Query the number of available seats/rooms/cars.
    protected int queryNum(int id, String key) {
        Trace.info("RM::queryNum(" + id + ", " + key + ") called.");
        ReservableItem curObj = (ReservableItem) readData(id, key);
        int value = 0;
        if (curObj != null) {
            value = curObj.getCount();
        }
        Trace.info("RM::queryNum(" + id + ", " + key + ") OK: " + value);
        return value;
    }

    // Query the price of an item.
    protected int queryPrice(int id, String key) {
        Trace.info("RM::queryCarsPrice(" + id + ", " + key + ") called.");
        ReservableItem curObj = (ReservableItem) readData(id, key);
        int value = 0;
        if (curObj != null) {
            value = curObj.getPrice();
        }
        Trace.info("RM::queryCarsPrice(" + id + ", " + key + ") OK: $" + value);
        return value;
    }

    // Reserve an item.
    protected boolean reserveItem(int id, int customerId,
                                  String key, String location) {
        Trace.info("RM::reserveItem(" + id + ", " + customerId + ", "
                + key + ", " + location + ") called.");
        // Read customer object if it exists (and read lock it).
        Customer cust = (Customer) readData(id, Customer.getKey(customerId));
        if (cust == null) {
            Trace.warn("RM::reserveItem(" + id + ", " + customerId + ", "
                   + key + ", " + location + ") failed: customer doesn't exist.");
            return false;
        }

        // Check if the item is available.
        ReservableItem item = (ReservableItem) readData(id, key);
        if (item == null) {
            Trace.warn("RM::reserveItem(" + id + ", " + customerId + ", "
                    + key + ", " + location + ") failed: item doesn't exist.");
            return false;
        } else if (item.getCount() == 0) {
            Trace.warn("RM::reserveItem(" + id + ", " + customerId + ", "
                    + key + ", " + location + ") failed: no more items.");
            return false;
        } else {
            // Do reservation.
            cust.reserve(key, location, item.getPrice());
            writeData(id, cust.getKey(), cust);

            // Decrease the number of available items in the storage.
            item.setCount(item.getCount() - 1);
            item.setReserved(item.getReserved() + 1);

            Trace.warn("RM::reserveItem(" + id + ", " + customerId + ", "
                    + key + ", " + location + ") OK.");
            return true;
        }
    }

    /**
     * Gets the id of a location, persisting a new location to the database if
     * necessary.
     *
     * @param connection The database connection (transaction) within which
     * to perform the insertion/selection.
     * @param location The name of the location to find or create.
     */
    private int getOrCreateLocation(Connection connection, String location)
    throws SQLException {
        final PreparedStatement stmt = connection.prepareStatement(
                "WITH new " +
                "AS ( " +
                "  INSERT INTO location ( name ) " +
                "  SELECT ? " +
                "  WHERE NOT EXISTS ( " +
                "    SELECT 1 " +
                "    FROM location l " +
                "    WHERE l.name = ? " +
                "  ) " +
                "  RETURNING id " +
                ") " +
                "SELECT id " +
                "FROM new " +
                "UNION " +
                "SELECT l.id " +
                "FROM location l " +
                "WHERE l.name = ? "
        );
        stmt.setString(1, location);
        stmt.setString(2, location);
        stmt.setString(3, location);

        final ResultSet rs = stmt.executeQuery();
        if(!rs.next())
            throw new RuntimeException("Unable to insert/select location.");

        final int id = rs.getInt(1);

        return id;
    }

    // Flight operations //

    // Create a new flight, or add seats to existing flight.
    // Note: if flightPrice <= 0 and the flight already exists, it maintains
    // its current price.
    @Override
    public boolean addFlight(int id, int flightNumber,
                             int numSeats, int flightPrice) {
        Trace.info("RM::addFlight(" + id + ", " + flightNumber
                + ", $" + flightPrice + ", " + numSeats + ") called.");

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            for(int i = 0; i < numSeats; i++) {
                final PreparedStatement stmt = connection.prepareStatement(
                        "INSERT INTO flight ( flight_number, price ) " +
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
                    "DELETE FROM flight " +
                    "WHERE flight_number = ? "
            );
            stmt.setInt(1, flightNumber);
            stmt.executeUpdate();

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
                    "SELECT COUNT(f.id) " +
                    "FROM flight f " +
                    "WHERE NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM flight_reservation fr " +
                    "        WHERE fr.flight_id = f.id " +
                    "      ) " +
                    "  AND f.flight_number = ? "
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
                    "SELECT MIN(f.price) " +
                    "FROM flight f " +
                    "WHERE NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM flight_reservation fr " +
                    "        WHERE fr.flight_id = f.id " +
                    "      ) " +
                    "  AND f.flight_number = ? "
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
        Trace.info("RM::addCars(" + id + ", " + location + ", "
                + numCars + ", $" + carPrice + ") called.");
        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final int locationId = getOrCreateLocation(connection, location);

            for(int i = 0; i < numCars; i++) {
                final PreparedStatement stmt = connection.prepareStatement(
                        "INSERT INTO car ( location_id, price ) " +
                        "VALUES ( ?, ? ) "
                );
                stmt.setInt(1, locationId);
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
                    "DELETE FROM car AS c " +
                    "USING location l " +
                    "WHERE l.id = c.location_id " +
                    "  AND l.name = ? "
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
                    "FROM car c, location l " +
                    "WHERE c.location_id = l.id " +
                    "  AND l.name = ? " +
                    "  AND NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM car_reservation cr " +
                    "        WHERE cr.car_id = c.id " +
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
                    "SELECT MIN(c.price) " +
                    "FROM car c, location l " +
                    "WHERE c.location_id = l.id " +
                    "  AND l.name = ? "
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

            final int locationId = getOrCreateLocation(connection, location);

            for(int i = 0; i < numRooms; i++) {
                final PreparedStatement stmt = connection.prepareStatement(
                        "INSERT INTO room ( location_id, price ) " +
                        "VALUES ( ?, ? ) "
                );
                stmt.setInt(1, locationId);
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
                    "DELETE FROM room AS c " +
                    "USING location l " +
                    "WHERE l.id = c.location_id " +
                    "  AND l.name = ? "
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
                    "FROM room c, location l " +
                    "WHERE c.location_id = l.id " +
                    "  AND l.name = ? " +
                    "  AND NOT EXISTS ( " +
                    "        SELECT 1 " +
                    "        FROM room_reservation cr " +
                    "        WHERE cr.room_id = c.id " +
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
                    "SELECT MIN(c.price) " +
                    "FROM room c, location l " +
                    "WHERE c.location_id = l.id " +
                    "  AND l.name = ? "
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
        throw new UnsupportedOperationException();
    }

    // Delete customer from the database.
    @Override
    public boolean deleteCustomer(int id, int customerId) {
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
                    "DELETE FROM customer AS c " +
                    "WHERE c.id = ? "
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
        Trace.info(
                String.format(
                    "RM::queryCustomerInfo(%d, %d)",
                    id,
                    customerId
                )
        );

        StringBuffer sb = new StringBuffer();
        int totalPrice = 0;

        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            final PreparedStatement flightStmt = connection.prepareStatement(
                    "SELECT f.flight_number, f.price " +
                    "FROM flight f, flight_reservation fr, customer cu " +
                    "WHERE fr.flight_id = f.id " +
                    "  AND fr.customer_id = cu.id " +
                    "  AND cu.id = ? "
            );
            flightStmt.setInt(1, customerId);

            final PreparedStatement carStmt = connection.prepareStatement(
                    "SELECT l.name, c.price " +
                    "FROM car c, car_reservation cr, location l, customer cu " +
                    "WHERE cr.car_id = c.id " +
                    "  AND cr.customer_id = cu.id " +
                    "  AND c.location_id = l.id " +
                    "  AND cu.id = ? "
            );
            carStmt.setInt(1, customerId);

            final PreparedStatement roomStmt = connection.prepareStatement(
                    "SELECT l.name, r.price " +
                    "FROM room r, room_reservation rr, location l, customer cu " +
                    "WHERE rr.room_id = r.id " +
                    "  AND rr.customer_id = cu.id " +
                    "  AND r.location_id = l.id " +
                    "  AND cu.id = ? "
            );
            roomStmt.setInt(1, customerId);

            final ResultSet flightRs = flightStmt.executeQuery();

            sb.append("Flights:\n");
            while(flightRs.next()) {
                final int flightNumber = flightRs.getInt(1);
                final int flightPrice = flightRs.getInt(2);
                totalPrice += flightPrice;
                sb.append(
                        String.format(
                            "#%d -- $%d.00\n",
                            flightNumber,
                            flightPrice
                        )
                );
            }

            final ResultSet carRs = carStmt.executeQuery();

            sb.append("Cars:\n");
            while(carRs.next()) {
                final String carLocation = carRs.getString(1);
                final int carPrice = carRs.getInt(2);
                totalPrice += carPrice;
                sb.append(
                        String.format(
                            "%s -- $%d.00\n",
                            carLocation,
                            carPrice
                        )
                );
            }

            final ResultSet roomRs = roomStmt.executeQuery();

            sb.append("Rooms:\n");
            while(roomRs.next()) {
                final String roomLocation = roomRs.getString(1);
                final int roomPrice = roomRs.getInt(2);
                totalPrice += roomPrice;
                sb.append(
                        String.format(
                            "%s -- $%d.00\n",
                            roomLocation,
                            roomPrice
                        )
                );
            }

            sb.append(String.format("Total: $%d.00\n", totalPrice));

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
                "INSERT INTO flight_reservation " +
                "            ( flight_id, customer_id ) " +
                "SELECT f.id, ? " +
                "FROM flight f " +
                "WHERE NOT EXISTS ( " +
                "        SELECT 1 " +
                "        FROM flight_reservation fr " +
                "        WHERE fr.flight_id = f.id " +
                "      ) " +
                "  AND f.flight_number = ? " +
                "ORDER BY f.price ASC " +
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
                "INSERT INTO car_reservation " +
                "            ( car_id, customer_id ) " +
                "SELECT c.id, ? " +
                "FROM car c, location l " +
                "WHERE NOT EXISTS ( " +
                "        SELECT 1 " +
                "        FROM car_reservation cr " +
                "        WHERE cr.car_id = c.id " +
                "      ) " +
                "  AND c.location_id = l.id " +
                "  AND l.name = ? " +
                "ORDER BY c.price ASC " +
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
                "INSERT INTO room_reservation " +
                "            ( room_id, customer_id ) " +
                "SELECT r.id, ? " +
                "FROM room r, location l " +
                "WHERE NOT EXISTS ( " +
                "        SELECT 1 " +
                "        FROM room_reservation rr " +
                "        WHERE rr.room_id = r.id " +
                "      ) " +
                "  AND r.location_id = l.id " +
                "  AND l.name = ? " +
                "ORDER BY r.price ASC " +
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
        try(final Connection connection = database.getConnection()) {
            connection.setAutoCommit(false);

            if(car) {
                final int rowCount = insertCarReservation(
                        connection,
                        customerId,
                        location
                ).executeUpdate();
                if(rowCount == 0)
                    return false;
            }

            if(room) {
                final int rowCount = insertRoomReservation(
                        connection,
                        customerId,
                        location
                ).executeUpdate();
                if(rowCount == 0)
                    return false;
            }

            for(Object o : flightNumbers) {
                Trace.info(o.toString());
                int flightNumber = Integer.parseInt((String)o);
                final int rowCount = insertFlightReservation(
                        connection,
                        customerId,
                        flightNumber
                ).executeUpdate();
                if(rowCount == 0)
                    return false;
            }

            connection.commit();
            return true;
        }
        catch(SQLException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }
}
