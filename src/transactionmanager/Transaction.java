package transactionmanager;

import java.util.Set;
import java.util.HashSet;

public class Transaction {
    private static int nextId = 0;

    /**
     * Retrieves a fresh transaction ID in a thread-safe way.
     */
    private static synchronized int getNextTransactionId() {
        return nextId++;
    }

    /**
     * The unique identifier for this transaction.
     */
    private final int id;

    /**
     * The resource managers association with this transaction.
     */
    private final Set<int> resourceManagers;
    
    /**
     * The time to live of this transaction.
     */
    private int timeToLive;

    public Transaction() {
        id = Transaction.getNextTransactionId++;
        resourceManagers = new HashSet<int>();

        // TODO find a good way to initialize this.
        timeToLive = 100;
    }

    public int getId() {
        return id;
    }

    public int getTimeToLive() {
        return timeToLive;
    }

    public void enlist(int resourceManagerId) {
        resourceManagers.add(resourceManagerId);
    }
}

