package transactionmanager;

import java.util.List;
import java.util.ArrayList;
import common.ResourceManager;
import common.NoSuchTransactionException;
import common.InvalidTransactionException;
import common.UncheckedThrow;
import common.Trace;
import common.SimulatedFailure;
import common.SimulatedFailureManager;
import java.util.Set;
import java.util.HashSet;

public class Transaction {
    public static final long DEFAULT_TTL = 60;

    private static int nextId = 0;

    /**
     * Retrieves a fresh transaction ID in a thread-safe way.
     */
    private static synchronized int getNextTransactionId() {
        return (int)Math.floor(Math.random()*100000);
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
    public void setState(Transaction.State state) {
        this.state = state;
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

        if(state != State.PENDING)
            throw new InvalidTransactionException(
                    String.format(
                        "Trying to commit a transaction in an " +
                        "invalid state (%s).",
                        state.toString()));

        state = State.PREPARED;

        try {
            for(final ResourceManager rm : resourceManagers)
                if(rm.commit(id))
                    preparedRMs.add(rm);
                else {
                    failed = true;
                    state = State.ABORTED;
                    break;
                }
        }
        catch(NoSuchTransactionException e) {
            throw UncheckedThrow.throwUnchecked(e);
        }

        Exception failure = null;

        if(SimulatedFailureManager.getInstance().getFailure() == SimulatedFailure.CRASH_AFTER_RECEIVE_TM) {
            System.exit(1); 
        } 

        for(final ResourceManager rm : resourceManagers) {
            try {
                if(failed)
                    rm.abort(id);
                else {
                    try {
                        rm.mergeCommit(id);
                    }
                    catch(Exception e) {
                        Trace.error(
                                String.format(
                                    "mergeCommit failed for an RM !"
                                )
                        );
                        failure = e;
                    }
                }
            }
            catch(Exception e) {
                failure = e;
            }
        }

        if(failure != null) {
            state = State.ABORTED;
            throw UncheckedThrow.throwUnchecked(failure);
        }

        state = State.COMMITTED;
    }

    public synchronized void abort() {
        if(state != State.PENDING && state != State.PREPARED)
            throw new InvalidTransactionException(
                    "Trying to commit a finished transaction."
            );

        Trace.info(String.format(
                    "Aborting transaction %d.",
                    this.getId()));

        Exception failure = null;

        state = State.ABORTED;

        for(final ResourceManager rm : resourceManagers) {
            try {
                rm.abort(id);
            }
            catch(Exception e) {
                // TODO use an ExceptionList ?
                failure = e;
            }
        }

        if(failure != null)
            throw UncheckedThrow.throwUnchecked(failure);
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

    private synchronized void prepare() {
        if(state != State.PENDING)
            throw new InvalidTransactionException(
                    String.format(
                        "Transaction state invariant violated in " +
                        "transaction %d",
                        id
                    )
            );

        boolean failed = false;

        try {
            for(final ResourceManager rm : resourceManagers)
                if(!rm.commit(id)) {
                    failed = true;
                    Trace.warn(
                            String.format(
                                "ResourceManager %s failed to enter " +
                                "prepared state for transaction %d",
                                rm.getClass().toString(),
                                id
                            )
                    );
                    break;
                }
        }
        catch(NoSuchTransactionException e) {
            // commit should never throw, so if an exception is raised, it's
            // probably because an RM is down, so we need to abort the
            // transaction.
            failed = true;
            Trace.warn(
                    String.format(
                        "Failed to prepare: An enlisted RM is unaware of " +
                        "transaction %d.",
                        id
                    )
            );
        }

        if(failed)
            abort(); // will set state = ABORTED
        else
            state = State.PREPARED;
    }

    public synchronized void partialCommit() {
        Trace.info(String.format(
                    "Performing partial commit for state %s for transaction %d.",
                    state.toString(),
                    this.getId()));

        if(state == State.PENDING) {
            prepare();
        }
        else if(state == State.PREPARED) {
            for(final ResourceManager rm : resourceManagers) {
                try {
                    rm.mergeCommit(id);
                    Trace.info(
                            String.format(
                                "Fully committed transaction %d on " +
                                "ResourceManager %s",
                                id,
                                rm.getClass().toString()
                            )
                    );
                }
                catch(Exception e) {
                    Trace.warn(
                            String.format(
                                "ResourceManager %s failed to enter " +
                                "committed state for transaction %d",
                                rm.getClass().toString(),
                                id
                            )
                    );
                }
            }

            state = State.COMMITTED;
        }
        else {
            throw new InvalidTransactionException(
                    String.format(
                        "Transaction %d is in state %s, which cannot be " +
                        "committed.",
                        state.toString()
                    )
            );
        }
    }

    public enum State {
        COMMITTED,
        ABORTED,
        PENDING,
        PREPARED
    }
}
