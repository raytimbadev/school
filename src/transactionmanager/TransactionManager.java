package transactionmanager;

import common.ResourceManager;
import common.NoSuchTransactionException;
import common.RedundantTransactionException;
import common.UncheckedThrow;

import java.util.Hashtable;
import java.util.Set;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class TransactionManager {
    final ScheduledExecutorService ttlChecker;

    /**
     * Maps transaction IDs to transactions.
     */
    private final Map<Integer, Transaction> transactionMap;

    public TransactionManager() {
        transactionMap = new Hashtable<Integer, Transaction>();
        ttlChecker = Executors.newScheduledThreadPool(1);
    }

    public synchronized Transaction start() {
        final Transaction txn = new Transaction();
        transactionMap.put(txn.getId(), txn);

        return txn;
    }

    /**
     * Commits the transaction associated with a given identifier.
     *
     * @param transactionId The identifier of the transaction to commit.
     * @throws NoSuchTransactionException The given identifier is invalid.
     * @return Whether the transaction is successfully committed.
     */
    public synchronized boolean commit(int transactionId)
    throws NoSuchTransactionException {
        final Transaction tx = transactionMap.get(transactionId);
        if(tx == null)
            throw new NoSuchTransactionException();

        tx.commit();
        transactionMap.remove(transactionId);

        return true;
    }

    /**
     * Aborts the transaction associated with a given identifier.
     *
     * Each resource manager associated with the transaction is contacted and
     * instructed to abort the transaction.
     *
     * @param transactionId The identifier of the transaction to abort.
     * @throws NoSuchTransactionException The given identifier is invalid.
     * @return Whether the transaction is successfully aborted.
     */
    public synchronized boolean abort(int transactionId)
    throws NoSuchTransactionException {
        final Transaction tx = transactionMap.get(transactionId);
        if(tx == null)
            throw new NoSuchTransactionException();

        final Set<ResourceManager> rms = tx.getResourceManagers();
        for(final ResourceManager rm : rms) {
            rm.commit(transactionId);
        }
        transactionMap.remove(transactionId);

        return true;
    }

    /**
     * Enlists a resource manager with a transaction.
     *
     * The given resource manager is added to the set of resource managers
     * associated with the transaction identified by the transaction ID.
     *
     * @param transactionId The identifier of the transaction.
     * @param rm The resource manager to associate with the transaction.
     * @throws NoSuchTransaction The transaction identifier is invalid.
     */
    public synchronized void enlist(int transactionId, ResourceManager rm)
    throws NoSuchTransactionException {
        final Transaction tx = transactionMap.get(transactionId);
        if(tx == null)
            throw new NoSuchTransactionException();
        tx.enlist(rm);

        try {
            rm.start(tx.getId());
        }
        catch(RedundantTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    private class TtlChecker implements Runnable {
        final Transaction transaction;

        public TtlChecker(Transaction transaction) {
            this.transaction = transaction;
        }

        @Override
        public void run() {
            synchronized(transaction) {
                // If the transaction is finished
                if(transaction.getState() != Transaction.State.PENDING)
                    // Then we don't do anything.
                    return;

                long ttl = transaction.getTimeToLive();

                // If the transaction has expired
                if(ttl <= 0)
                    // abort it on all associated RMs
                    transaction.abort();

                // Else, the transaction has not expired. Someone must have
                // used `touch` behind our backs! Pesky clients.
                // We just schedule another check after ttl seconds elapse.
                // That'll show them.
                ttlChecker.schedule(
                        new TtlChecker(transaction), ttl, TimeUnit.SECONDS);
            }
        }
    }
}
