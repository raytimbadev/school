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

public class DeleteItemOperation implements Operation<Boolean> {
    int id;
    int customerId;

    public DeleteItemOperation(int id, int customerId) {
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
}