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

public class DeleteFlightOperation implements Operation<Boolean> {
    int id;
    int flightNumber;

    public DeleteFlightOperation(int id, int flightNumber) {
        this.id = id;
        this.flightNumber = flightNumber;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(flightNumber);
        return l;
    }

    @Override
    public Boolean invoke(BasicDataSource database) {
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
}

