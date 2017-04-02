package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;

public class ReserveCarOperation extends TransactionOperation<Boolean> {
    final int customerId;
    final String location;

    public ReserveCarOperation(
            TransactionDataStore data,
            int id,
            int customerId,
            String location) {
        super(data, id, LockType.LOCK_WRITE);
        this.customerId = customerId;
        this.location = location;
    }

    @Override
    public Boolean invoke() {
        ItemGroup g = getDatum(location);
        if(g == null)
            return false;
        return g.reserve(customerId); 
    }

}
