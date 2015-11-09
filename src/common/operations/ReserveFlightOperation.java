package common.operations;

import java.util.List;
import java.util.ArrayList;

public class ReserveFlightOperation implements Operation<Boolean> {
    int id;
    int customerId;
    int flightNumber;

    public ReserveFlightOperation(int id, int customerId, int flightNumber) {
        this.id = id;
        this.customerId = customerId;
        this.flightNumber = flightNumber;
    }

    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(customerId);
        l.add(flightNumber);
        return l;
    }

    @Override
    public Boolean invoke() {
        return null;
    }
}
