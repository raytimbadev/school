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

public class DeleteFlightOperation extends TransactionOperation<Boolean> {
    int id;
    int flightNumber;

    public DeleteFlightOperation(
            TransactionDataStore data,
            int id,
            int flightNumber) {
        super(data, id, LockType.LOCK_WRITE);

        this.flightNumber = flightNumber;
    }

    @Override
    public Boolean invoke() {
        final String key = String.valueOf(flightNumber);
        final ItemGroup g = getDatum(key);
        
        if(g == null) {
            return false; 
        }

        if(g.getReservedCount() == 0) {
            removeDatum(key);
            return true;
        }

        return false;
    }
}

