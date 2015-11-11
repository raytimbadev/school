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


public class ReserveRoomOperation implements Operation<Boolean> {
    int id;
    int customerId;
    String location;

    public ReserveRoomOperation(int id, int customerId, String location) {
        this.id = id;
        this.customerId = customerId;
        this.location = location;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        l.add(location);
        return l;
    }

    @Override
    public Boolean invoke(Hashtable<String, ItemGroup> data) {
        ItemGroup g  = data.get(location);
        return g.reserve(customerId); 
    }

}

