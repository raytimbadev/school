package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;

public class DeleteCustomerOperation extends TransactionOperation<Boolean> {
    int customerId;

    public DeleteCustomerOperation(
            TransactionDataStore data,
            int id,
            int customerId) {
        super(data, id, LockType.LOCK_WRITE);

        this.customerId = customerId;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(customerId);
        final ItemGroup g = getDatum(key);

        if(g == null){
            return false;
        }

        removeDatum(key);
        return true;
    }
}
