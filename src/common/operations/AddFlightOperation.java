package common.operations;
import common.*;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
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

        super(tds, id);

        this.flightNumber = flightNumber;
        this.numSeats = numSeats;
        this.flightPrice = flightPrice;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(flightNumber);

        ItemGroup g = data.get(key, LockType.LOCK_WRITE);

        if(g == null) {
            g = new ItemGroup("flight", key, numSeats, flightPrice);
            data.put(key, g);
        }
        else {
            if(flightPrice > 0)
                g.setPrice(flightPrice);
            g.setCount(g.getCount() + numSeats);
        }

        return true;
    }
}
