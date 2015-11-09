package common.operations;

import java.util.List;
import java.util.ArrayList;

public class DeleteRoomsOperation implements Operation<Boolean> {
    int id;
    String location;

    public DeleteRoomsOperation(int id, String location) {
        this.id = id;
        this.location = location;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(location);
        return l;
    }

    @Override
    public Boolean invoke() {
        return null;
    }
}
