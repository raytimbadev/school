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

public class AddFlightOperation implements Operation<Boolean> {
    int id;
    int flightNumber;
    int numSeats;
    int flightPrice;

    public AddFlightOperation(int id, int flightNumber, int numSeats,
            int flightPrice) {
        this.id = id;
        this.flightNumber = flightNumber;
        this.numSeats = numSeats;
        this.flightPrice = flightPrice;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(flightNumber);
        l.add(numSeats);
        l.add(flightPrice);
        return l;
    }

    @Override
    public Boolean invoke(BasicDataSource database) {
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
}
