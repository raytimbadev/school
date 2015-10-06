package common.sockets;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

public class NetworkCaller {
    final InetAddress address;
    final int port;

    public NetworkCaller(
            InetAddress address,
            int port) {
        this.address = address;
        this.port = port;
    }

    public Response invoke(final Request request)
    throws IOException, ClassNotFoundException {
        System.out.println("Invoking network call.");

        final Socket socket = new Socket(address, port);

        final ObjectOutputStream oos =
            new ObjectOutputStream(socket.getOutputStream());
        oos.writeObject(request);

        System.out.println("Send request.");

        final ObjectInputStream ois =
            new ObjectInputStream(socket.getInputStream());
        final Response response = (Response)ois.readObject();

        System.out.println("Got response.");

        return response;
    }
}
