package lockmanager;

public class Lock {
    final String datumName;
    final Transaction transaction;
    final LockType lockType;

    public Lock(String datumName, Transaction transaction, LockType lockType) {
        this.datumName = datumName;
        this.transaction = transaction;
        this.lockType = lockType;
    }

    @Override
    public int hashCode() {
        return datumName.hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if(o == null)
            return false;

        if(o instanceof Lock)
            return o.datumName == this.datumName
                && o.transaction.equals(this.transaction)
                && o.lockType.equals(this.lockType);

        return false;
    }
}
