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

public class DeleteCarsOperation extends TransactionOperation<Boolean> {
    String location;

    public DeleteCarsOperation(
            TransactionDataStore data,
            int id,
            String location) {
        super(data, id, LockType.LOCK_WRITE);

        this.location = location;
    }

    @Override
    public Boolean invoke() {
        final String key = location;

        final ItemGroup g = getDatum(key);
        
        if(g == null){
            return true; 
        }

        if(g.getReservedCount() == 0) {
            removeDatum(key);
            return true;
        }

        return false;
    }
}
