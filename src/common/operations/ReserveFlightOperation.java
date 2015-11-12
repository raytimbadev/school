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

public class ReserveFlightOperation extends TransactionOperation<Boolean> {
    int customerId;
    int flightNumber;

    public ReserveFlightOperation(
            TransactionDataStore data
            int id,
            int customerId,
            int flightNumber) {
        super(data, id, LockType.LOCK_WRITE);
        this.customerId = customerId;
        this.flightNumber = flightNumber;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(flightNumber);
        ItemGroup g  = getDatum(key);
        return g.reserve(customerId); 
    }

}
