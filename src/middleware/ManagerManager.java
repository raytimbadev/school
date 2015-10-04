package middleware;

public class ManagerManager {
    private static ManagerManager instance;

    public static void initialize(
            ResourceManager flightManager,
            ResourceManager carManager,
            ResourceManager roomManager
    ) {
        if (instance != null)
            throw new RuntimeException(
                    "Tried to initialize initialized singleton."
            );

        instance = new ManagerManager(
                flightManager,
                carManager,
                roomManager
        );
    }

    public static ManagerManager getInstance() {
        if (instance == null)
            throw new RuntimeException(
                    "Tried to get instance of uninitialized singleton."
            );

        return instance;
    }

    private ResourceManager flightManager, carManager, roomManager;

    private ManagerManager(
            ResourceManager flightManager,
            ResourceManager carManager,
            ResourceManager roomManager
    ) {
        this.flightManager = flightManager;
        this.carManager = carManager;
        this.roomManager = roomManager;
    }

    public ResourceManager getFlightManager() {
        return flightManager;
    }

    public ResourceManager getCarManager() {
        return carManager;
    }

    public ResourceManager getRoomManager() {
        return roomManager;
    }
}
