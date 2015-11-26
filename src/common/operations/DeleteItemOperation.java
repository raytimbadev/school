package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

public class DeleteItemOperation extends TransactionOperation<Boolean> {
    int customerId;

    public DeleteItemOperation(
            TransactionDataStore data,
            int id,
            int customerId) {
        super(data, id, LockType.LOCK_WRITE);

        this.customerId = customerId;
    }

    @Override
    public Boolean invoke() {
        final TransactionDataStore data = getDataStore();

        for(final String k : data.keys()) {
            ItemGroup g = null;
            if(this.isTransaction())
                g = data.get(k, LockType.LOCK_READ);
            else
                g = data.get(k);

            if(g.hasReservation(customerId)) {
                if(this.isTransaction())
                    g = data.get(k, LockType.LOCK_WRITE);
                g.cancel(customerId);
            }
        }

        return true;
    }
}
