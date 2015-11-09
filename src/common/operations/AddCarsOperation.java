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


public class AddCarsOperation implements Operation<Boolean> {
    int id;
    String location;
    int count;
    int price;

    public AddCarsOperation(int id, String location, int count, int price) {
        this.id = id;
        this.location = location;
        this.count = count;
        this.price = price;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(location);
        l.add(count);
        l.add(price);
        return l;
    }

    @Override
    public Boolean invoke(BasicDataSource database) {
        try(final Connection connection = database.getConnection()) {
        connection.setAutoCommit(false);

        for(int i = 0; i < count; i++) {
            final PreparedStatement stmt = connection.prepareStatement(
                    "INSERT INTO item ( location, price ) " +
                    "VALUES ( ?, ? ) "
            );
            stmt.setString(1, location);
            stmt.setInt(2, price);
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
