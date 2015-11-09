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
    private final Set<ResourceManager> resourceManagers;
    
    /**
     * The time to live of this transaction.
     */
    private int timeToLive;

    public Transaction() {
        id = Transaction.getNextTransactionId();
        resourceManagers = new HashSet<Integer>();

        // TODO find a good way to initialize this.
        timeToLive = 100;
    }

    public int getId() {
        return id;
    }

    public int getTimeToLive() {
        return timeToLive;
    }

    public void enlist(ResourceManager rm) {
        resourceManagers.add(rm);
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if(o == null)
            return false;

        if(o instanceof Transaction) {
            final Transaction p = (Transaction)o;
            return p.id == this.id;
        }

        return false;
    }
}

