package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;

public class QueryFlightOperation extends TransactionOperation<Integer> {
    int flightNumber;

    public QueryFlightOperation(
            TransactionDataStore data,
            int id,
            int flightNumber) {
        super(data, id, LockType.LOCK_READ);
        this.flightNumber = flightNumber;
    }

    @Override
    public Integer invoke() {
        ItemGroup g = getDatum(String.valueOf(flightNumber));
        if(g == null)
            throw new RuntimeException(
                    String.format("No such flight: %d.", flightNumber));
        else
            return g.getAvailableCount();
    }
}
