package transactionmanager;

import common.ResourceManager;

import java.util.Hashtable;
import java.util.Set;
import java.util.Map;

public class TransactionManager {
    /**
     * Maps transaction IDs to transactions.
     */
    private Map<Integer, Transaction> transactionMap;

    public TransactionManager() {
        transactionMap = new Hashtable<Integer, Transaction>();
    }

    public synchronized Transaction start() {
        final Transaction txn = new Transaction();
        transactionMap.put(txn.getId(), txn);
        return txn;
    }

    /**
    * Commits a the transaction associated with the passed ID's
    * It does this by sending the commit signal to the appropriate
    * middleware
    */
    public synchronized boolean commit(int transactionId)
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

    public synchronized void enlist(int transactionId, ResourceManager rm)
    throws NoSuchTransactionException {
        final Transaction tx = transactionMap.get(transactionId);
        if(tx == null)
            throw new NoSuchTransactionException();
        tx.enlist(rm);
    }
}
