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

public class DeleteItemOperation extends TransactionOperation<Boolean> {
    int customerId;

    public DeleteItemOperation(
            TransactionDataStore data,
            int id,
            int customerId) {
        super(data, id, LockType.LOCK_WRITE);

        this.customerId = customerId;
    }

    @Override
    public Boolean invoke() {
        for(final ItemGroup g : this.values())
            g.cancel(customerId);

        return true;
    }
