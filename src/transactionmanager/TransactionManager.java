package transactionmanager;

import java.util.Hashtable;
import java.util.Map;

public class TransactionManager {
    /**
     * Maps transaction IDs to transactions.
     */
    private Map<int, Transaction> transactionMap;

    public TransactionManager() {
        transactionMap = new Hashtable<int, Transaction>();
    }

    public synchronized Transaction begin() {
        final Transaction txn = new Transaction();
        transactionMap.put(txn.getId(), txn);
        return txn;
    }

    public synchronized void commit(int transactionId) {
        
    }

    public synchronized void abort(int transactionId) {

    }
}
