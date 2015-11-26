package transactionmanager;

import common.ResourceManager;
import common.NoSuchTransactionException;
import common.InvalidTransactionException;
import common.UncheckedThrow;
import common.Trace;

import java.util.Set;
import java.util.HashSet;

public class Transaction {
    public static final long DEFAULT_TTL = 60;

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

    private State state;

    /**
     * The resource managers associated with this transaction.
     */
    private final Set<ResourceManager> resourceManagers;

    /**
     * The time to live of this transaction.
     */
    private long lastTouch;

    /**
     * The number of seconds since a last touch event to consider this
     * transaction expired.
     */
    private final long timeToLive;

    /**
     * Initializes a new transaction, allocating a new transaction ID for it
     * from a synchronized counter, and giving it the default time to live.
     */
    public Transaction() {
        id = Transaction.getNextTransactionId();
        resourceManagers = new HashSet<ResourceManager>();
        timeToLive = DEFAULT_TTL;
        state = State.PENDING;

        touch();
    }

    /**
     * Initializes a new transaction, allocating a new transaction ID for it
     * from a synchronized counter, and using a given time to live in seconds.
     */
    public Transaction(long ttl) {
        id = Transaction.getNextTransactionId();
        resourceManagers = new HashSet<ResourceManager>();
        timeToLive = ttl;
        state = State.PENDING;

        touch();
    }

    /**
     * Gets the time to live for this transaction.
     *
     * If this value is at most zero, then the transaction is expired.
     */
    public long getTimeToLive() {
        final long now = System.currentTimeMillis();
        // expired: now - lastTouch > timeToLive * 1000
        return (lastTouch - now + timeToLive * 1000) / 1000;
    }

    /**
     * Touches this transaction.
     */
    public void touch() {
        if(state != State.PENDING)
            throw new InvalidTransactionException(
                    "Trying to modify a finished transaction."
            );

        lastTouch = System.currentTimeMillis();
    }

    public int getId() {
        return id;
    }

    public Transaction.State getState() {
        return state;
    }

    public void enlist(ResourceManager rm) {
        if(state != State.PENDING)
            throw new InvalidTransactionException(
                    "Trying to modify a finished transaction."
            );

        Trace.info(String.format(
                    "Enlisting ResourceManager to transaction %d.",
                    this.getId()));
        if(!resourceManagers.contains(rm))
            resourceManagers.add(rm);
    }

    public synchronized void commit() {
        if(state != State.PENDING)
            throw new InvalidTransactionException(
                    "Trying to commit a finished transaction."
            );

        Trace.info(String.format(
                    "Committing transaction %d.",
                    this.getId()));

        try {
            for(final ResourceManager rm : resourceManagers)
                rm.commit(id);
        }
        catch(NoSuchTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }

        state = State.COMMITTED;
    }

    public synchronized void abort() {
        if(state != State.PENDING)
            throw new InvalidTransactionException(
                    "Trying to commit a finished transaction."
            );

        Trace.info(String.format(
                    "Aborting transaction %d.",
                    this.getId()));

        try {
            for(final ResourceManager rm : resourceManagers)
                rm.abort(id);
        }
        catch(NoSuchTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }

        state = State.ABORTED;
    }

    public Set<ResourceManager> getResourceManagers() {
        return resourceManagers;
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

    public enum State {
        COMMITTED,
        ABORTED,
        PENDING
    }
}

