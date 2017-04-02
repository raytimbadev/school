package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

public class DoesCustomerExistOperation extends TransactionOperation<Boolean> {
    int customerId;

    public DoesCustomerExistOperation(
            TransactionDataStore data,
            int id,
            int customerId) {
        super(data, id, LockType.LOCK_READ);
        this.customerId = customerId;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(customerId);
        final ItemGroup g = getDatum(key);
        return g != null;
    }
}
