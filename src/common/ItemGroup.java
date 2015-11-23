package common;

import java.util.Map;
import java.util.HashMap;

public class ItemGroup {
    private int itemCount;
    private int reservedCount;
    private HashMap<Integer, Integer> reservations;
    private int price;
    private final String key;
    private final String itemType;

    public ItemGroup(
            String itemType,
            String key,
            int itemCount,
            int price) {
        this.itemType = itemType;
        this.key = key;
        this.itemCount = itemCount;
        this.price = price;
        reservations = new HashMap<Integer, Integer>();
        reservedCount = 0;
    }

    public void setCount(int count) {
        if(itemCount < 0)
            throw new RuntimeException("Item count must be nonnegative.");
        if(itemCount < reservedCount)
            throw new RuntimeException(
                    "New item count would be less than number of reservations."
            );
        this.itemCount = count;
    }

    public int getCount() {
        return itemCount;
    }

    public void setPrice(int price) {
        if(price < 0)
            throw new RuntimeException("Item count must be nonnegative.");
        this.price = price;
    }

    public int getPrice() {
        return price;
    }

    /**
     * Reserves one item for a given customer.
     *
     * @param customerId The ID of the customer for which to reserve the item.
     * @return The total number of items now reserved by that customer.
     */
    public synchronized boolean reserve(int customerId) {
        if(reservedCount == itemCount)
            return false;

        Integer currentCount = reservations.get(customerId);
        if(currentCount == null)
            currentCount = 0;
        currentCount++;
        reservedCount++;

        reservations.put(customerId, currentCount);
        return true;
    }

    /**
     * Deletes all reservations for a given customer.
     *
     * @param customerId The ID of the customer for which to delete
     * reservations.
     */
    public synchronized void cancel(int customerId) {
        Trace.info(String.format(
                    "Cancelling reservation(s) for customer %d.",
                    customerId));

        final Integer currentCount = reservations.get(customerId);
        if(currentCount != null) {
            reservations.remove(customerId);
            reservedCount -= currentCount;
            Trace.info(String.format(
                        "Deleted %d reservations for customer %d.",
                        currentCount,
                        customerId));
        }
        else
            Trace.warn(String.format(
                        "Customer %d has no reservation(s).",
                        customerId));
    }

    public synchronized boolean hasReservation(int customerId) {
        final Integer currentCount = reservations.get(customerId);
        return currentCount != null && currentCount != 0;
    }

    public int getReservedCountFor(int customerId) {
        final Integer n = reservations.get(customerId);
        if(n == null)
            return 0;
        else
            return n;
    }

    public int getReservedCount() {
        return reservedCount;
    }

    public int getAvailableCount() {
        return itemCount - reservedCount;
    }

    public String getKey() {
        return key;
    }

    public String getItemType() {
        return itemType;
    }

    @Override
    public Object clone() {
        final ItemGroup o = new ItemGroup(itemType, key, itemCount, price);
        o.reservedCount = reservedCount;
        o.reservations = (HashMap<Integer, Integer>)reservations.clone();
        return o;
    }
}
