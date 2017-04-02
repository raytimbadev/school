package common.sockets;

import common.ResourceManager;

import java.net.Socket;
import java.net.InetAddress;

public class RequestContext {
    final ResourceManager resourceManager;
    final Socket clientSocket;
    final int requestNumber;

    public RequestContext(ResourceManager rm, Socket client, int number) {
        resourceManager = rm;
        clientSocket = client;
        requestNumber = number;
    }

    public ResourceManager getResourceManager() {
        return resourceManager;
    }

    public Socket getClientSocket() {
        return clientSocket;
    }

    public int getRequestNumber() {
        return requestNumber;
    }

    public static class RequestContextBuilder {
        ResourceManager resourceManager;
        Socket clientSocket;
        int requestNumber;

        public RequestContextBuilder() {
        }

        public RequestContextBuilder withResourceManager(ResourceManager rm) {
            resourceManager = rm;
            return this;
        }

        public RequestContextBuilder withClientSocket(Socket client) {
            clientSocket = client;
            return this;
        }

        public RequestContextBuilder withRequestNumber(int number) {
            requestNumber = number;
            return this;
        }

        public RequestContext build() {
            return new RequestContext(
                    resourceManager,
                    clientSocket,
                    requestNumber
            );
        }
    }
}
