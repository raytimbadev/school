package common.operations;

import java.util.List;
import java.util.ArrayList;

public class NewCustomerIdOperation implements Operation<Boolean> {
    int id;
    int customerId;

    public NewCustomerIdOperation(int id, int customerId) {
        this.id = id;
        this.customerId = customerId;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        return l;
    }

    @Override
    public Boolean invoke() {
        return null;
    }
}
