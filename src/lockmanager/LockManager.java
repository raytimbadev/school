package lockmanager;

import common.Trace;
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
            LockType lockType) {
        Map<Integer, Lock> transactionMap = lockMap.get(datumName);
        final Lock lock = new Lock(datumName, transaction, lockType);

        Trace.info(String.format(
                    "Locking %s",
                    datumName));

        if(transactionMap == null) {
            Trace.info(String.format(
                        "Creating transaction map: %s -> %d -> Lock",
                        datumName,
                        transaction));

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

            Trace.info(String.format(
                        "Grant WRITE lock for %s to %d",
                        datumName,
                        transaction));

            return lock;

        }
        else if(lockType == LockType.LOCK_READ) {
            while(!canAcquireRead(transaction, transactionMap)) {
                try {
                    wait();
                }
                catch(InterruptedException e) {
                    return null;
                }
            }

            if(transactionMap.get(transaction) == null) {
                transactionMap.put(transaction, lock);

                Trace.info(String.format(
                            "Grant READ lock for %s to %d",
                            datumName,
                            transaction));
            }
            else
                Trace.info(String.format(
                            "Existing lock for %s to %d",
                            datumName,
                            transaction));

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
            final Integer transaction,
            Map<Integer, Lock> transactionMap)
    {
        for(final Lock lock : transactionMap.values())
            if(lock.lockType == LockType.LOCK_WRITE)
                return lock.getTransaction() == transaction;

        return true;
    }

    private synchronized boolean canAcquireWrite(
            final Integer transaction,
            Map<Integer, Lock> transactionMap) {
        for(final Lock lock : transactionMap.values())
            if(lock.getTransaction() == transaction)
                continue;
            else
                return false;


        return true;

    }
}
