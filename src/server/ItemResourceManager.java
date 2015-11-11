// -------------------------------
// Adapted from Kevin T. Manley
// CSE 593
// -------------------------------

package server;

import common.*;
import lockmanager.*;  
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

public class ItemResourceManager extends DatabaseResourceManager {
    public ItemResourceManager() {
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
        DeleteItemOperation op = new DeleteItemOperation(id,customerId);
        if(id == -1) {
            op.invoke(mainData);
        }

        final List<Operation> txData = transactions.get(id);
        if(ops == null)
            throw UncheckedThrow.throwUnchecked(
                    new NoSuchTransactionException(id)
            );
        lockManager.lock(String.valueOf(customerId),id,LockType.LOCK_WRITE);
        op.invoke(txData);

    }

    // Return a bill.
    @Override
    public String queryCustomerInfo(int id, int customerId) {
        Trace.info(
                String.format(
                    "RM::queryCustomerInfo(%d, %d)",
                    id,
                    customerId
                )
        );

        QueryCustomerInfoOperation op =
            new QueryCustomerInfoOperation(id,customerId);

        if(id == -1) {
            op.invoke(mainData);
        }

        final List<Operation> txData = transactions.get(id);

        if(txData == null)
            throw UncheckedThrow.throwUnchecked(
                    new NoSuchTransactionException(id)
            );

        lockManager.lock(String.valueOf(customerId),id,LockType.LOCK_WRITE);

        return op.invoke(txData);
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
