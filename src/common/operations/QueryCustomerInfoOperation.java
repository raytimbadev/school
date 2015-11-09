package common.operations;
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

import java.util.List;
import java.util.ArrayList;

public class QueryCustomerInfoOperation implements Operation<String> {
    int id;
    int customerId;

    public QueryCustomerInfoOperation(int id, int customerId) {
        this.id = id;
        this.customerId = customerId;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        return l;
    }

    @Override
    public String invoke(BasicDataSource database) {
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
}
