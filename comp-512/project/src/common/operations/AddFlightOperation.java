package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;

public class AddFlightOperation extends TransactionOperation<Boolean> {
    int flightNumber;
    int numSeats;
    int flightPrice;

    public AddFlightOperation(
            TransactionDataStore tds, 
            int id, 
            int flightNumber,
            int numSeats,
            int flightPrice) {

        super(tds, id, LockType.LOCK_WRITE);

        this.flightNumber = flightNumber;
        this.numSeats = numSeats;
        this.flightPrice = flightPrice;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(flightNumber);

        ItemGroup g = getDatum(key);

        if(g == null) {
            g = new ItemGroup("flight", key, numSeats, flightPrice);
            putDatum(key, g);
        }
        else {
            if(flightPrice > 0)
                g.setPrice(flightPrice);
            g.setCount(g.getCount() + numSeats);
        }

        return true;
    }
}
