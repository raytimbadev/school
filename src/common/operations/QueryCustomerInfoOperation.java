package common.operations;

import common.*;
import lockmanager.LockType;

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

public class QueryCustomerInfoOperation extends TransactionOperation<String> {
    int customerId;

    public QueryCustomerInfoOperation(
            TransactionDataStore data,
            int id,
            int customerId) {
        super(data, id, LockType.LOCK_READ);
        this.customerId = customerId;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        return l;
    }

    @Override
    public String invoke() {
        final StringBuffer sb = new StringBuffer();

        for(ItemGroup g : this.values()) {
            final int reserved = g.getReservedCountFor(customerId);
            if(reserved > 0)
                sb.append(String.format(
                            "%2d %s in %s @ %d.00$\n",
                            reserved,
                            g.getItemType(),
                            g.getKey(),
                            g.getPrice())
                );
        }

        return sb.toString();
    }
}
