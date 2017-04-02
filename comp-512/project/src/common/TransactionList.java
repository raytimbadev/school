package common;

import java.io.Serializable;
import java.util.List;
import java.util.ArrayList;

public class TransactionList
    extends ArrayList<Integer>
    implements Serializable, List<Integer> {

    public TransactionList() {
    }
}
