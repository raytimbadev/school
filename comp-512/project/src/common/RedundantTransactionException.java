package common;

public class RedundantTransactionException extends Exception {
    private Integer transactionId;

    public RedundantTransactionException() {
        transactionId = null;
    }

    public RedundantTransactionException(int transactionId) {
        this.transactionId = transactionId;
    }

    @Override
    public String toString() {
        if(transactionId == null)
            return "Redundant transaction request.";
        else
            return "Request to create transaction " +
                String.valueOf(transactionId) +
                " is redundant.";
    }
}
