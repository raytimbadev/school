package lockmanager;

public class LockTimeoutException extends RuntimeException {
    public LockTimeoutException(String message) {
        super(message);
    }
}
