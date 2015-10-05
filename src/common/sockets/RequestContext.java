package common.sockets;

import common.ResourceManager;

import java.net.Socket;
import java.net.InetAddress;

public class RequestContext {
    final ResourceManager resourceManager;
    final Socket clientSocket;

    public RequestContext(ResourceManager rm, Socket client) {
        resourceManager = rm;
        clientSocket = client;
    }

    public ResourceManager getResourceManager() {
        return resourceManager;
    }

    public Socket getClientSocket() {
        return clientSocket;
    }

    public static class RequestContextBuilder {
        ResourceManager resourceManager;
        Socket clientSocket;

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

        public RequestContext build() {
            return new RequestContext(resourceManager, clientSocket);
        }
    }
}
