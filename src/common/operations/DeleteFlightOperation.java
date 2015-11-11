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

public class DeleteFlightOperation implements Operation<Boolean> {
    int id;
    int flightNumber;

    public DeleteFlightOperation(int id, int flightNumber) {
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
    public Boolean invoke(Hashtable<String, ItemGroup> data) {
        final String key = String.valueOf(flightNumber);
        final ItemGroup g = data.get(key);
        
        if(g == null) {
            return true; 
        }

        if(g.getReservedCount() == 0) {
            data.remove(key);
            return true;
        }

        return false;
    }
}

