package server;

import common.ItemGroup;
import lockmanager.LockManager;
import lockmanager.LockType;

import java.util.Map;
import java.util.Hashtable;

public class TransactionDataStore {
    final Map<String, ItemGroup> mainData;
    final Map<String, ItemGroup> txData;
    final LockManager lockManager;
    final int transactionId;

    public TransactionDataStore(
            int transactionId,
            LockManager lockManager,
            Map<String, ItemGroup> mainData) {
        this.transactionId = transactionId;
        this.mainData = mainData;
        this.lockManager = lockManager;

        txData = new Hashtable<String, ItemGroup>();
    }

    public synchronized ItemGroup get(String key, LockType lockType) {
        // take out the necessary lock on the data
        lockManager.lock(key, transactionId, lockType);

        // fetch from the transaction local storage
        final ItemGroup txGroup = txData.get(key);

        // if we have this group in the transaction-local storage, return it
        if(txGroup != null)
            return txGroup;

        // fetch from global storage
        final ItemGroup mainGroup = mainData.get(key);

        if(mainGroup == null)
            return null;
        
        // make a copy of the main data
        final ItemGroup newTxGroup = (ItemGroup)mainGroup.clone();

        // store it in the transaction-local storage
        txData.put(key, newTxGroup);

        // return the transaction-local copy
        return newTxGroup;
    }
    
}
