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


public class ReserveRoomOperation extends TransactionOperation<Boolean> {
    int customerId;
    String location;

    public ReserveRoomOperation(
            TransactionDataStore data,
            int id,
            int customerId,
            String location) {
        super(data, id, LockType.LOCK_WRITE);
        this.customerId = customerId;
        this.location = location;
    }

    @Override
    public Boolean invoke() {
        ItemGroup g  = getDatum(location);
        return g.reserve(customerId); 
    }

}

