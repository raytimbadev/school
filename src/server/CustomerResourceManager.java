// -------------------------------
// Adapted from Kevin T. Manley
// CSE 593
// -------------------------------

package server;
import lockmanager.*; 
import common.*;
import lockmanager.*; 
import transactionmanager.Transaction;
import common.operations.*; 
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

public class CustomerResourceManager extends DatabaseResourceManager {
    public CustomerResourceManager() {
    }

    // Customer operations //

    // Delete customer from the database.
    @Override
    public boolean deleteCustomer(int id, int customerId) {
        Trace.info(
                String.format(
                    "RM::deleteCustomer(%d, %d)",
                    id,
                    customerId
                )
        );
        DeleteCustomerOperation op = new DeleteCustomerOperation(id,customerId);
        return new DeleteCustomerOperation(
            getTransactionData(id),
            id,
            customerId)
        .invoke();
    }

    // Return a bill.
    @Override
    public String queryCustomerInfo(int id, int customerId) {
        Trace.warn(
                String.format(
                    "RM::queryCustomerInfo(%d, %d)",
                    id,
                    customerId
                )
        );

        throw new UnsupportedOperationException();
    }

    // Reserve an itinerary.
    @Override
    public boolean reserveItinerary(
            int id,
            int customerId,
            Vector flightNumbers,
            String location,
            boolean car,
            boolean room) {
        throw new UnsupportedOperationException();
    }
}
