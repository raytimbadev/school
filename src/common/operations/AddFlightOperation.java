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

public class AddFlightOperation implements Operation<Boolean> {
    int id;
    int flightNumber;
    int numSeats;
    int flightPrice;

    public AddFlightOperation(int id, int flightNumber, int numSeats,
            int flightPrice) {
        this.id = id;
        this.flightNumber = flightNumber;
        this.numSeats = numSeats;
        this.flightPrice = flightPrice;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(flightNumber);
        l.add(numSeats);
        l.add(flightPrice);
        return l;
    }

    @Override
    public Boolean invoke(Hashtable<String, ItemGroup> data) {
        final String key = String.valueOf(flightNumber);

        ItemGroup g = data.get(key);
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
