package lockmanager;

import transactionmanager.Transaction;

public class Lock {
    final String datumName;
    final Transaction transaction;
    final LockType lockType;

    public Lock(String datumName, Transaction transaction, LockType lockType) {
        this.datumName = datumName;
        this.transaction = transaction;
        this.lockType = lockType;
    }

    public Transaction getTransaction() {
        return transaction;
    }

    @Override
    public int hashCode() {
        return datumName.hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if(o == null)
            return false;

        if(o instanceof Lock) {
            final Lock p = (Lock)o;
            return p.datumName == this.datumName
                && p.transaction.equals(this.transaction)
                && p.lockType.equals(this.lockType);
        }

        return false;
    }
}
