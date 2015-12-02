package transactionmanager;

import common.ResourceManager;
import common.NoSuchTransactionException;
import common.RedundantTransactionException;
import common.UncheckedThrow;
import common.Trace;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
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

    /**
     * Maps transaction IDs for finished transactions to their status.
     */
    private final Map<Integer, Transaction.State> doneTransactionMap;

    public TransactionManager() {
        transactionMap = new HashMap<Integer, Transaction>();
        ttlChecker = Executors.newScheduledThreadPool(1);
        doneTransactionMap = new HashMap<Integer, Transaction.State>();
    }

    public synchronized Transaction start() {
        final Transaction txn = new Transaction();
        transactionMap.put(txn.getId(), txn);

        // schedule a TTL check for the transaction.
        scheduleTtlCheck(txn);

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

        if(tx.getState() == Transaction.State.PREPARED) {
            throw new RuntimeException();
        }

        tx.commit();
        transactionMap.remove(transactionId);
        doneTransactionMap.put(transactionId, Transaction.State.COMMITTED);

        return true;
    }

    public synchronized boolean partialCommit(int transactionId)
    throws NoSuchTransactionException {
        final Transaction tx = transactionMap.get(transactionId);
        if(tx == null)
            throw new NoSuchTransactionException();

        if(tx.getState() == Transaction.State.PREPARED) {
            transactionMap.remove(transactionId);
            doneTransactionMap.put(transactionId, Transaction.State.COMMITTED);
        }
        tx.partialCommit();

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

        doneTransactionMap.put(transactionId, Transaction.State.ABORTED);
        transactionMap.remove(transactionId);
        tx.abort();

        return true;
    }

    /**
     * Enlists a resource manager with a transaction.
     *
     * The given resource manager is added to the set of resource managers
     * associated with the transaction identified by the transaction ID.
     *
     * If the resource manager is already enlisted, then this method will
     * simply touch the transaction, resetting its TTL.
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
        tx.touch();

        // if the transaction is not newly enlisting the rm, then return
        // right away to avoid calling start twice on the rm.
        if(!tx.enlist(rm))
            return;

        try {
            rm.start(tx.getId());
        }
        catch(RedundantTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }
    }

    public Transaction.State getTransactionStatus(int id) {
        final Transaction.State doneTransactionStatus =
            doneTransactionMap.get(id);
        final Transaction tx =
            transactionMap.get(id);

        if(tx != null)
            return tx.getState();

        return doneTransactionStatus;
    }

    private void scheduleTtlCheck(Transaction tx) {
        long ttl = tx.getTimeToLive();

        Trace.info(String.format(
                    "Scheduling TTL check for transaction %d in %d seconds.",
                    tx.getId(),
                    ttl));

        ttlChecker.schedule(
                new TtlChecker(tx),
                ttl,
                TimeUnit.SECONDS);
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
                if(ttl <= 0) {
                    Trace.warn(String.format(
                                "Transaction %d timed out.",
                                transaction.getId()));
                    // abort it on all associated RMs
                    transaction.abort();
                }
                else
                    // Else, the transaction has not expired. Someone must have
                    // used `touch` behind our backs! Pesky clients.
                    // We just schedule another check after ttl seconds elapse.
                    // That'll show them.
                    scheduleTtlCheck(transaction);
            }
        }
    }
}
