package common.operations;

import common.*;
import lockmanager.LockType;

import java.beans.PropertyVetoException;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.ArrayList;


public class AddCarsOperation extends TransactionOperation<Boolean> {
    String location;
    int count;
    int price;

    public AddCarsOperation(
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
    public Boolean invoke() {
        final String key = location;

        ItemGroup g = getDatum(key);

        if(g == null) {
            g = new ItemGroup("car", key, count, price);
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
