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

    /**
     * Marks a resource manager as enlisted in this transaction and decides
     * whether that resource manager is newly enlisted or redundantly so.
     */
    public boolean enlist(ResourceManager rm) {
        if(state != State.PENDING)
            throw new InvalidTransactionException(
                    "Trying to modify a finished transaction."
            );

        Trace.info(String.format(
                    "Enlisting ResourceManager to transaction %d.",
                    this.getId()));

        final boolean result = resourceManagers.contains(rm);
        resourceManagers.add(rm);
        return !result;
    }

    public synchronized void commit() {
        Trace.info(String.format(
                    "Committing %s transaction %d.",
                    state.toString(),
                    this.getId()));

        List<ResourceManager> preparedRMs = new ArrayList<ResourceManager>();
        boolean failed = false;

        try {
            for(final ResourceManager rm : resourceManagers)
                if(state == State.PENDING) {
                    if(rm.commit(id))
                        preparedRMs.add(rm);
                    else {
                        failed = true;
                        break;
                    }
                }
                else
                    throw new InvalidTransactionException(
                            String.format(
                                "Trying to commit a transaction in an " +
                                "invalid state (%s).",
                                state.toString()));
        }
        catch(NoSuchTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }

        Exception failure = null;

        for(final ResourceManager rm : preparedRMs) {
            try {
                if(failed)
                    rm.abort(id);
                else
                    rm.mergeCommit(id);
            }
            catch(Exception e) {
                failure = e;
            }
        }

        if(failure != null)
            throw UncheckedThrow.throwUnchecked(failure);
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

