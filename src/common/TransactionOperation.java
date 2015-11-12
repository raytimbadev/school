package common;

import lockmanager.LockType;

public abstract class TransactionOperation<T> implements Operation<T> {
    public static final int NO_TRANSACTION = -1;

    private final int id;
    private final LockType lockType;
    private final TransactionDataStore data;

    public TransactionOperation(
            TransactionDataStore tds,
            int id) {
        this.lockType = null;
        this.data = tds;
        this.id = id;
    }

    public TransactionOperation(
            TransactionDataStore tds,
            int id,
            LockType lockType) {
        this.lockType = lockType;
        this.data = tds;
        this.id = id;
    }

    public ItemGroup getDatum(String key) {
        if(this.isTransaction())
            return data.get(key, lockType);
        else
            return data.get(key);
    }

    public Set<ItemGroup> values() {
        if(this.isTransaction())
            return data.values();
        else
            return data.unsafeValues();
    }

    public void putDatum(String key, ItemGroup value) {
        if(this.isTransaction())
            data.put(key, value);
        else
            data.unsafePut(key, value);
    }

    public int getTransactionId() {
        return id;
    }

    public boolean isTransaction() {
        return id != NO_TRANSACTION;
    }

    public LockType getLockType() {
        return lockType;
    }
}
