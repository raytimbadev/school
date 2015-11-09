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

public class NewCustomerIdOperation implements Operation<Boolean> {
    int id;
    int customerId;

    public NewCustomerIdOperation(int id, int customerId) {
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
    public Boolean invoke(BasicDataSource database) {
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
}
