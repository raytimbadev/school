package common.sockets;

import java.io.Serializable;

public class Response implements Serializable {
    public static Response success(Object result) {
        return new Response(result, null, true);
    }

    public static Response error(Throwable error) {
        return new Response(null, error, false);
    }
    Object result;
    Throwable error;

    boolean success;

    private Response(Object result, Throwable error, boolean success) {
        this.result = result;
        this.error = error;
        this.success = success;
    }

    public boolean isSuccessful() {
        return success;
    }

    public Object getResult() {
        return result;
    }

    public Throwable getError() {
        return error;
    }
}
