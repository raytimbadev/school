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
    private final Map<String, Map<Integer, Lock>> lockMap;

    public LockManager() {
        lockMap = new Hashtable<String, Map<Integer, Lock>>();
    }

    public synchronized Lock lock(
            String datumName,
            int transaction,
            LockType lockType)
    {
        Map<Integer, Lock> transactionMap = lockMap.get(datumName);
        final Lock lock = new Lock(datumName, transaction, lockType);

        if(transactionMap == null) {
            transactionMap = new Hashtable<Integer, Lock>();
            lockMap.put(datumName, transactionMap);
        }

        if(lockType == LockType.LOCK_WRITE) {
            while(!canAcquireWrite(transaction, transactionMap)) {
                try {
                    wait();
                }
                catch(InterruptedException e) {
                    return null;
                }
            }

            transactionMap.put(transaction, lock);
            return lock;

        }
        else if(lockType == LockType.LOCK_READ) {
            while(!canAcquireRead(transactionMap)) {
                try {
                    wait();
                }
                catch(InterruptedException e) {
                    return null;
                }
            }

            transactionMap.put(transaction, lock);
            return lock;
        }

        return null;
    }

    public synchronized void releaseTransaction(Integer transaction) {
        for(final Map<Integer, Lock> transactionMap : lockMap.values())
            transactionMap.remove(transaction);
        notifyAll();
    }

    private synchronized boolean canAcquireRead(
            Map<Integer, Lock> transactionMap)
    {
        if(transactionMap.isEmpty())
            return true;

        for(final Lock lock : transactionMap.values())
            if(lock.lockType == LockType.LOCK_WRITE)
                return false;

        return true;
    }

    private synchronized boolean canAcquireWrite(
        final Integer transaction,
        Map<Integer, Lock> transactionMap)
    {
        for(final Lock lock : transactionMap.values())
            if(lock.lockType != LockType.LOCK_READ
                    || lock.getTransaction() != transaction)
                return false;

        return true;

    }
}
