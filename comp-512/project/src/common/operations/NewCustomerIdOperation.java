package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;

public class NewCustomerIdOperation extends TransactionOperation<Boolean> {
    int id;
    int customerId;

    public NewCustomerIdOperation(
            TransactionDataStore data,
            int id,
            int customerId) {
        super(data, id, LockType.LOCK_WRITE);
        this.customerId = customerId;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(customerId);

        ItemGroup g = getDatum(key);
        if(g == null) {
            g = new ItemGroup("customer", key, 0, 0);
            putDatum(key, g);
        }
        else { //customer already exists
            return false;
        }
        return true;
    }
}
