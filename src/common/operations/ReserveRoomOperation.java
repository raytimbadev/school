package common.operations;

import java.util.List;
import java.util.ArrayList;

public class ReserveRoomOperation implements Operation<Boolean> {
    int id;
    int customerId;
    String location;

    public ReserveRoomOperation(int id, int customerId, String location) {
        this.id = id;
        this.customerId = customerId;
        this.location = location;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        l.add(location);
        return l;
    }

    @Override
    public Boolean invoke() {
        return null;
    }
}

