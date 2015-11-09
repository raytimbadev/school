package common.operations;

import java.util.List;
import java.util.ArrayList;

public class NewCustomerOperation implements Operation<Integer> {
    int id;

    public NewCustomerOperation(int id) {
        this.id = id;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        return l;
    }

    @Override
    public Integer invoke() {
        return null;
    }
}
