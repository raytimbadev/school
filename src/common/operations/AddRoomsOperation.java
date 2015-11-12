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

public class AddRoomsOperation extends TransactionOperation<Boolean> {
    String location;
    int count;
    int price;

    public AddRoomsOperation(
            TransactionDataStore data,
            int id,
            String location,
            int count,
            int price) {
        super(data, id, LockType.LOCK_WRITE);

        this.location = location;
        this.count = count;
        this.price = price;
    }

    @Override
    public Boolean invoke(Hashtable<String, ItemGroup> data) {
        final String key = location;

        ItemGroup g = getDatum(key);

        if(g == null) {
            g = new ItemGroup("room", key, count, price);
            putDatum(key, g);
        }
        else {
            if(price > 0)
                g.setPrice(price);
            g.setCount(g.getCount() + count);
        }

        return true;
    }
}

