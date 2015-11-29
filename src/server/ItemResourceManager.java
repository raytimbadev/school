// -------------------------------
// Adapted from Kevin T. Manley
// CSE 593
// -------------------------------

package server;

import client.SocketResourceManager;
import common.*;
import lockmanager.*;
import common.operations.*;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

public class ItemResourceManager extends DatabaseResourceManager {
    public ItemResourceManager(String dbname,ResourceManager middleware) {
        super(dbname,middleware);
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
        touch(id);
        return new DeleteItemOperation(
            getTransactionData(id),
            id,
            customerId)
        .invoke();
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

        touch(id);
        return new QueryCustomerInfoOperation(
            getTransactionData(id),
            id,
            customerId)
        .invoke();
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
