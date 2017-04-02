package lockmanager;

public class Lock {
    final String datumName;
    final int transaction;
    final LockType lockType;

    public Lock(String datumName, int transaction, LockType lockType) {
        this.datumName = datumName;
        this.transaction = transaction;
        this.lockType = lockType;
    }

    public int getTransaction() {
        return transaction;
    }

    public String getDatumName() {
        return datumName;
    }

    public LockType getLockType() {
        return lockType;
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
                && p.transaction == this.transaction
                && p.lockType.equals(this.lockType);
        }

        return false;
    }
}
