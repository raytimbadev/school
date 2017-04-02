package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;

public class QueryFlightPriceOperation extends TransactionOperation<Integer> {
    int flightNumber;

    public QueryFlightPriceOperation(
            TransactionDataStore data,
            int id,
            int flightNumber) {
        super(data, id, LockType.LOCK_READ);
        this.flightNumber = flightNumber;
    }

    @Override
    public Integer invoke() {
        final String key = String.valueOf(flightNumber);
        final ItemGroup g = getDatum(key);
        if(g == null)
            throw new RuntimeException("No such location '" + key + "'.");
        else
            return g.getPrice();
    }
}
