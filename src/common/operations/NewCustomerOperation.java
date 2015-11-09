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

public class NewCustomerOperation implements Operation<Integer> {
    int id;

    public NewCustomerOperation(int id) {
        this.id = id;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        return l;
    }

    @Override
    public Integer invoke(BasicDataSource database) {
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
}
