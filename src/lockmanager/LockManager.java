package lockmanager;

import common.Trace;
import transactionmanager.Transaction;

import java.util.Hashtable;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class LockManager {
    /**
     * The default timeout used for lock requests.
     */
    public static final long DEFAULT_LOCK_REQUEST_TTL = 30;

    /**
     * Maps row IDs to locks.
     */
    private final Map<String, Map<Integer, Lock>> lockMap;

    /**
     * Set of finished transactions.
     */
    private final Set<Integer> finishedTransactions;

    /**
     * The timeout for lock requests.
     */
    private final long requestTtl;

    /**
     * The service that schedules timeout checks.
     */
    private final ScheduledExecutorService ttlChecker;

    public LockManager() {
        lockMap = new Hashtable<String, Map<Integer, Lock>>();
        finishedTransactions = new HashSet<Integer>();
        ttlChecker = Executors.newScheduledThreadPool(1);
        requestTtl = DEFAULT_LOCK_REQUEST_TTL;
    }

    public synchronized void markFinished(int transaction) {
        finishedTransactions.add(transaction);
    }

    public synchronized Lock lock(
            String datumName,
            int transaction,
            LockType lockType) {
        Map<Integer, Lock> transactionMap = lockMap.get(datumName);
        final Lock lock = new Lock(datumName, transaction, lockType);

        Trace.info(String.format(
                    "Trying to lock item %s for transaction %d",
                    datumName,
                    transaction));

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
                    block(lock);
                }
                catch(InterruptedException e) {
                    throw new LockTimeoutException(
                            String.format(
                                "Lock request for %s to %d timed out.",
                                datumName,
                                transaction
                            )
                    );
                }
            }

            transactionMap.put(transaction, lock);

            Trace.info(String.format(
                        "Grant WRITE lock for item %s to transaction %d",
                        datumName,
                        transaction));

            return lock;

        }
        else if(lockType == LockType.LOCK_READ) {
            while(!canAcquireRead(transaction, transactionMap)) {
                try {
                    block(lock);
                }
                catch(InterruptedException e) {
                    throw new LockTimeoutException(
                            String.format(
                                "Lock request for item %s to transaction " +
                                "%d timed out.",
                                datumName,
                                transaction
                            )
                    );
                }
            }

            if(transactionMap.get(transaction) == null) {
                transactionMap.put(transaction, lock);

                Trace.info(String.format(
                            "Grant READ lock for item %s to transaction %d",
                            datumName,
                            transaction));
            }
            else
                Trace.info(String.format(
                            "Using existing lock for item %s of " +
                            "transaction %d",
                            datumName,
                            transaction));

            return lock;
        }

        throw new InvalidLockException(
                String.format(
                    "Lock request for item %s to transaction %d reached " +
                    "impossible state.",
                    datumName,
                    transaction
                )
        );
    }

    /**
     * Calls wait with a timeout.
     */
    private void block(Lock lock) throws InterruptedException {
        final LockRequestCanceller lrc = 
            new LockRequestCanceller(Thread.currentThread(), lock);
        ttlChecker.schedule(
                lrc,
                requestTtl,
                TimeUnit.SECONDS
        );
        wait();
        lrc.cancel();
        // just to be sure there are no race conditions
        if(Thread.currentThread().isInterrupted())
            throw new InterruptedException();
    }

    public synchronized void releaseTransaction(int transaction) {
        // remove all locks owned by the transaction
        for(final Map<Integer, Lock> transactionMap : lockMap.values())
            transactionMap.remove(transaction);
        // mark the transaction as finished to cause any blocked threads to
        // abort.
        markFinished(transaction);
        notifyAll();
    }

    private synchronized boolean canAcquireRead(
            final Integer transaction,
            Map<Integer, Lock> transactionMap)
    {
        if(finishedTransactions.contains(transaction))
            throw new InvalidLockException(
                    String.format(
                        "No lock can be granted to finished transaction %d.",
                        transaction
                    )
            );

        for(final Lock lock : transactionMap.values())
            if(lock.lockType == LockType.LOCK_WRITE)
                return lock.getTransaction() == transaction;

        return true;
    }

    private synchronized boolean canAcquireWrite(
            final Integer transaction,
            Map<Integer, Lock> transactionMap) {
        if(finishedTransactions.contains(transaction))
            throw new InvalidLockException(
                    String.format(
                        "No lock can be granted to finished transaction %d.",
                        transaction
                    )
            );

        for(final Lock lock : transactionMap.values())
            if(lock.getTransaction() == transaction)
                continue;
            else
                return false;


        return true;

    }

    private class LockRequestCanceller implements Runnable {
        final Lock lock;
        final Thread thread;
        boolean cancelled;

        public LockRequestCanceller(Thread thread, Lock lock) {
            this.thread = thread;
            this.lock = lock;
            this.cancelled = false;
        }

        public synchronized void cancel() {
            cancelled = true;
        }

        private synchronized void killThread() {
            // don't kill the thread if this canceller was cancelled
            if(cancelled) return;
            thread.interrupt();
        }

        public void run() {
            killThread();
        }
    }
}
