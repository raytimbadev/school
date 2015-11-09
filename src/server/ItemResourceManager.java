// -------------------------------
// Adapted from Kevin T. Manley
// CSE 593
// -------------------------------

package server;

import common.*;
import lockmanager.*;  

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

public class ItemResourceManager extends DatabaseResourceManager {
    public ItemResourceManager(
            String dbUsername,
            String dbPassword,
            String dbUrl
    ) throws IOException, SQLException, PropertyVetoException {
        super(dbUsername, dbPassword, dbUrl);
    }

    // Customer operations //

    // Delete customer from the database.
    @Override
    public boolean deleteCustomer(int id, int customerId, int transaction) {
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
                    "DELETE FROM item_reservation AS ir " +
                    "WHERE ir.customer_id = ? "
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
    public String queryCustomerInfo(int id, int customerId, int transaction) {
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

    // Reserve an itinerary.
    @Override
    public boolean reserveItinerary(
            int id,
            int customerId,
            Vector flightNumbers,
            String location,
            boolean car,
            boolean room,
	    int transaction) {
        throw new UnsupportedOperationException();
    }
   //start a transaction
    @Override
    public int start() {
        throw new UnsupportedOperationException(); 
	}

	//commit a transaction
	@Override
	public boolean commit(int id) {
		throw new UnsupportedOperationException(); 
	}
	
	//abort a transaction
	@Override
	public boolean abort(int id) {
		throw new UnsupportedOperationException(); 
	}
}
