package common.operations;

import java.util.List;
import java.util.ArrayList;

public class QueryFlightPriceOperation implements Operation<Integer> {
    int id;
    int flightNumber;

    public QueryFlightPriceOperation(int id, int flightNumber) {
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
    public Integer invoke() {
        return null;
    }
}
