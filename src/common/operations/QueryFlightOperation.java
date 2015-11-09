package common.operations;

import java.util.List;
import java.util.ArrayList;

public class QueryFlightOperation implements Operation<Boolean> {
    int id;
    int flightNumber;

    public QueryFlightOperation(int id, int flightNumber) {
        this.id = id;
        this.flightNumber = flightNumber;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(flightNumber);
        return l;
    }

    @Override
    public Boolean invoke() {
        return null;
    }
}
