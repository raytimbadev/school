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

import java.util.List;
import java.util.ArrayList;

public class ReserveCarOperation implements Operation<Boolean> {
    int id;
    int customerId;
    String location;

    public ReserveCarOperation(int id, int customerId, String location) {
        this.id = id;
        this.customerId = customerId;
        this.location = location;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        l.add(location);
        return l;
    }

    @Override
    public Boolean invoke(BasicDataSource database) {
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

}
