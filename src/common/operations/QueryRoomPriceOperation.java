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

public class QueryRoomPriceOperation implements Operation<Integer> {
    int id;
    String location;

    public QueryRoomPriceOperation(int id, String location) {
        this.id = id;
        this.location = location;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(location);
        return l;
    }

    @Override
    public Integer invoke(BasicDataSource database) {
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
}
