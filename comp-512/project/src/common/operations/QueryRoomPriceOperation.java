package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;

public class QueryRoomPriceOperation extends TransactionOperation<Integer> {
    String location;

    public QueryRoomPriceOperation(
            TransactionDataStore data,
            int id,
            String location) {
        super(data, id, LockType.LOCK_READ);
        this.location = location;
    }

    @Override
    public Integer invoke() {
        ItemGroup g = getDatum(location);
        if(g == null)
            throw new RuntimeException("No such location '" + location + "'.");
        else
            return g.getPrice();
    }
}
