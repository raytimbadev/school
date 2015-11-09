package common.operations;

import java.util.List;
import java.util.ArrayList;

public class AddFlightOperation implements Operation<Boolean> {
    int id;
    int flightNumber;
    int numSeats;
    int flightPrice;

    public AddFlightOperation(int id, int flightNumber, int numSeats,
            int flightPrice) {
        this.id = id;
        this.flightNumber = flightNumber;
        this.numSeats = numSeats;
        this.flightPrice = flightPrice;
    }

    @Override
    public List<Object> getParameters() {
        final List<Object> l = new ArrayList<Object>();
        l.add(id);
        l.add(flightNumber);
        l.add(numSeats);
        l.add(flightPrice);
        return l;
    }

    @Override
    public Boolean invoke() {
        return null;
    }
}
