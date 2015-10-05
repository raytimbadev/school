package client;

import common.ResourceManager;

import java.net.URL;
import java.net.MalformedURLException;

public class SocketClient {

    ResourceManager proxy;
    
    public SocketClient(String host, int port) {
        proxy = new SocketResourceManager(host, port);
    }
}
