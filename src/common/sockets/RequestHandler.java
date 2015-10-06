package common.sockets;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.InputStream;
import java.net.Socket;

public class RequestHandler implements Runnable {
    final RequestContext requestContext;

    public RequestHandler(RequestContext ctx) {
        requestContext = ctx;
    }

    public void run() {
        System.out.println("Handling request.");
        Response response;

        try {
            final Socket sock = requestContext.getClientSocket();
            final InputStream input = sock.getInputStream();

            try {
                // Receive the Request
                final Request request = Request.RequestParser.parseStream(input);

                final Object result =
                    request.invoke(requestContext.getResourceManager());
                response = Response.success(result);
                System.out.println("Successful.");
            }
            catch(Exception e) {
                response = Response.error(e);
                System.out.println("Failure.");
            }

            final ObjectOutputStream output =
                new ObjectOutputStream(sock.getOutputStream());
            output.writeObject(response);

            sock.close();
        }
        catch(IOException e) {
            return;
        }
    }
}
