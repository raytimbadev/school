package common;

import lockmanager.LockManager;
import lockmanager.LockType;

import java.util.Map;
import java.util.Hashtable;
import java.util.HashSet;
import java.util.Set;
import java.util.Collection;

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

    /**
     * Writes all data from the transaction-local storage to the global
     * storage.
     */
    public synchronized void merge() {
        mainData.putAll(txData);
    }

    /**
     * Fetches an ItemGroup from the main storage, making no considerations for
     * transactions.
     *
     * @param key The key of the datum to fetch.
     * @return The requested datum, or null if it doesn't exist.
     */
    public synchronized ItemGroup get(String key) {
        return mainData.get(key);
    }

    /**
     * Overwrites an the ItemGroup associated with a given key in a
     * transaction-oriented way, by acquiring the necessary lock and using the
     * transaction-local storage.
     *
     * @param key The key of the datum to write.
     * @param value The value to write for the key.
     * @param lockType The type of lock to request for the key.
     */
    public synchronized void put(
            String key,
            ItemGroup value) {
        lockManager.lock(
                key,
                transactionId,
                LockType.LOCK_WRITE);

        txData.put(key, value);
    }

    public synchronized Collection<ItemGroup> values() {
        return txData.values();
    }

    public synchronized Set<String> keys() {
        Set<String> keys = new HashSet(txData.keySet());
        keys.addAll(mainData.keySet());
        return keys;
    }

    public synchronized Collection<ItemGroup> unsafeValues() {
        return mainData.values();
    }

    /**
     * Writes a key-value pair to the main data, with no consideration for
     * transactions.
     *
     * @param key The key to write.
     * @param value The value to write.
     */
    public synchronized void unsafePut(
            String key,
            ItemGroup value) {
        mainData.put(key, value);
    }

    /**
     * Fetches an ItemGroup in a transaction-oriented way, by acquiring the
     * necessary lock and using the transaction-local storage.
     *
     * @param key The key of the datum to fetch.
     * @param lockType The type of lock to request on the datum.
     * @return The requested datum, or null if it doesn't exist.
     */
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
