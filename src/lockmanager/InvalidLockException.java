package lockmanager;

public class InvalidLockException extends RuntimeException {
    public InvalidLockException(String message) {
        super(message);
    }
}
