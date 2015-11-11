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

public class QueryCarsOperation implements Operation<Integer> {
    int id;
    String location;

    public QueryCarsOperation(int id, String location) {
        this.id = id;
        this.location = location;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(location);
        return l;
    }

    @Override
    public Integer invoke(Hashtable<String, ItemGroup> data) {
        ItemGroup g = data.get(location);
        if(g == null)
            throw new RuntimeException("No such location '" + location + "'.");
        else
            return g.getAvailableCount();
    }
}

