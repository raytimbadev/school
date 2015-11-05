package lockmanager;

import transactionmanager.Transaction;

import java.util.Hashtable;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

public class LockManager {
    /**
     * Maps row IDs to locks.
     */
    private final Map<String, Map<Transaction, Lock>> lockMap;

    public LockManager() {
        lockMap = new Hashtable<String, Map<Transaction, Lock>>();
    }

    public synchronized Lock lock(
            String datumName,
            Transaction transaction,
            LockType lockType)
    {
        final Map<Transaction, Lock> transactionMap = lockMap.get(datumName);
        final Lock lock = new Lock(datumName, transaction, lockType);

        if(transactionMap == null) {
            transactionMap = new Hashtable<Transaction, Lock>();
            lockMap.put(transaction, transactionMap);
        }

        if(lockType == LockType.LOCK_WRITE) {
            while(!canAcquireWrite(transaction, transactionMap))
                wait();

            transactionMap.put(transaction, lock);
            return lock;
        }
        else if(lockType == LockType.LOCK_READ) {
            while(!canAcquireRead(transactionMap))
                wait();

            transactionMap.put(transaction, lock);
            return lock;
        }

        return null;
    }

    public synchronized Lock releaseTransaction(Transaction transaction) {
        for(final Map<Transaction, Lock> transactionMap : lockMap.values())
            transactionMap.remove(transaction);
        notifyAll();
    }

    private synchronized boolean canAcquireRead(
            Map<Transaction, Lock> transactionMap)
    {
        if(transactionMap.isEmpty())
            return true;

        for(final Lock lock : transactionMap.values())
            if(lock.lockType == LockType.LOCK_WRITE)
                return false;

        return true;
    }

    private synchronized boolean canAcquireWrite(
        final Transaction transaction,
        Map<Transaction, Lock> transactionMap)
    {
        for(final Lock lock : transactionMap.values())
            if(lock.lockType != LockType.LOCK_READ
                    || ! lock.getTransaction().equals(transaction))
                return false;

        return true;

    }
}

