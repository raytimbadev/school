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

public class NewCustomerOperation implements Operation<Integer> {
    int id;

    public NewCustomerOperation(int id) {
        this.id = id;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        return l;
    }

    @Override
    public Boolean invoke(Hashtable<String, ItemGroup> data) {
        int max =0;
        String customerId="";

        for(Enumeration<String> e = v.keys; e.hasMoreElements();) {
            int temp = Integer.parseInt(e.nextElement());
            max = max > temp ? max, temp;
        }
        customerId = string.value(max+1); 
        final String key = customerId;

        ItemGroup g = data.get(key);
        if(g == null) {
            g = new ItemGroup("customer", key, 0, 0);
            data.put(key, g);
        }
        else { //customer already exists
            throw new RuntimeException("attempted to create existing customer");
        }
        return true;
    }
}
