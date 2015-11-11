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
    public Integer invoke(Hashtable<String, ItemGroup> data) {
        int max =0;

        for(Enumeration<String> e = data.keys(); e.hasMoreElements();) {
            int temp = Integer.parseInt(e.nextElement());
            max = max > temp ? max:temp;
        }
        final int customerId = max + 1;
        String key = String.valueOf(customerId); 

        ItemGroup g = new ItemGroup("customer", key, 0, 0);
        data.put(key, g);
        return customerId;
    }
}
