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

public class DeleteCustomerOperation extends TransactionOperation<Boolean> {
    int id;
    int customerId;

    public DeleteCustomerOperation(
            TransactionDataStore data,
            int id,
            int customerId) {
        super(data, id, LockType.LOCK_WRITE);

        this.customerId = customerId;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(id);

        ItemGroup g = getDatum(key);
        
        if(g==null){
            return true; 
        }

        removeDatum(key);
        return true;
    }
}
