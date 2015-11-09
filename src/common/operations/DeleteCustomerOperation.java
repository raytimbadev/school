package common.operations;
import org.apache.commons.dbcp2.BasicDataSource;
import java.util.List;
import java.util.ArrayList;

public class DeleteCustomerOperation implements Operation<Boolean> {
    int id;
    int customerId;

    public DeleteCustomerOperation(int id, int customerId) {
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
    public Boolean invoke(BasicDataSource database) {
        return null;
    }
}
