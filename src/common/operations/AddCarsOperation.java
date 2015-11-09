package common.operations;

import java.util.List;
import java.util.ArrayList;

public class AddCarsOperation implements Operation<Boolean> {
    int id;
    String location;
    int count;
    int price;

    public AddCarsOperation(int id, String location, int count, int price) {
        this.id = id;
        this.location = location;
        this.count = count;
        this.price = price;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(location);
        l.add(count);
        l.add(price);
        return l;
    }

    @Override
    public Boolean invoke() {
        return null;
    }
}
