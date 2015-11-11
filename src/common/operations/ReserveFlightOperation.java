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

public class ReserveFlightOperation implements Operation<Boolean> {
    int id;
    int customerId;
    int flightNumber;

    public ReserveFlightOperation(int id, int customerId, int flightNumber) {
        this.id = id;
        this.customerId = customerId;
        this.flightNumber = flightNumber;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        l.add(flightNumber);
        return l;
    }

    @Override
    public Boolean invoke(Hashtable<String, ItemGroup> data) {
        final String key = String.valueOf(flightNumber);
        ItemGroup g  = data.get(key);
        return g.reserve(customerId); 
    }

}
