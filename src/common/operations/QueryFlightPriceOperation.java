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

public class QueryFlightPriceOperation implements Operation<Integer> {
    int id;
    int flightNumber;

    public QueryFlightPriceOperation(int id, int flightNumber) {
        this.id = id;
        this.flightNumber = flightNumber;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(flightNumber);
        return l;
    }

    @Override
    public Integer invoke(Hashtable<String, ItemGroup> data) {
        final String key = String.valueOf(flightNumber);
        final ItemGroup g = data.get(key);
        if(g == null)
            throw new RuntimeException("No such location '" + key + "'.");
        else
            return g.getPrice();
    }
}
