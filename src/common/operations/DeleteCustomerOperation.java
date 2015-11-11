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

public class DeleteCustomerOperation implements Operation<Boolean> {
    int id;
    int customerId;

    public DeleteCustomerOperation(int id, int customerId) {
        this.id = id;
        this.customerId = customerId;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        return l;
    }

    @Override
    public Boolean invoke(Hashtable<String, ItemGroup> data) {
        final String key = String.valueOf(id);
        ItemGroup g = data.get(key);
        
        if(g==null){
            return true; 
        }

        data.remove(key);
        return true;
    }
}
