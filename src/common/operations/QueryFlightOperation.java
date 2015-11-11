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
import org.apache.commons.dbcp2.BasicDataSource;

import java.util.List;
import java.util.ArrayList;

public class QueryFlightOperation implements Operation<Integer> {
    int id;
    int flightNumber;

    public QueryFlightOperation(int id, int flightNumber) {
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
        ItemGroup g = data.get(String.valueOf(flightNumber));
        if(g == null)
            throw new RuntimeException(
                    String.format("No such flight: %d.", flightNumber));
        else
            return g.getAvailableCount();
    }
}
