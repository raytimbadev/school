package middleware;

import common.ResourceManager;
import common.sockets.RequestHandler;
import common.sockets.RequestContext;

import client.SocketResourceManager;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Middleware implements Runnable {


    public static final int BACKLOG_MULTIPLIER = 5;

    public final int THREAD_COUNT;

    final ExecutorService executorService;
    final ServerSocket serverSocket;
    final ResourceManager resourceManager;

    public Middleware(int port, InetAddress bindAddress, ResourceManager manager)
    throws IOException {
        resourceManager = manager;

        THREAD_COUNT = Runtime.getRuntime().availableProcessors();
        executorService = Executors.newFixedThreadPool(THREAD_COUNT);

        serverSocket = new ServerSocket(
                port,
                BACKLOG_MULTIPLIER * THREAD_COUNT,
                bindAddress
        );
    }

    @Override
    public void run() {
        boolean running = true;

        while(running) {
            try {
                final Socket clientSocket = serverSocket.accept();
                final RequestContext ctx =
                    new RequestContext.RequestContextBuilder()
                        .withResourceManager(resourceManager)
                        .withClientSocket(clientSocket)
                        .build();
                final RequestHandler handler = new RequestHandler(ctx);

                executorService.submit(handler);
            }
            catch(IOException e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args)
    throws Exception {
        final InetAddress bindAddress = InetAddress.getByName(args[0]);
        final int port = Integer.parseInt(args[1]);

        final InetAddress flightAddress = InetAddress.getByName(args[2]);
        final int flightPort = Integer.parseInt(args[3]);
        final ResourceManager flightManager =
            new SocketResourceManager(flightAddress, flightPort);

        final InetAddress carAddress = InetAddress.getByName(args[4]);
        final int carPort = Integer.parseInt(args[5]);
        final ResourceManager carManager =
            new SocketResourceManager(carAddress, carPort);

        final InetAddress roomAddress = InetAddress.getByName(args[6]);
        final int roomPort = Integer.parseInt(args[7]);
        final ResourceManager roomManager =
            new SocketResourceManager(roomAddress, roomPort);
	
	    final InetAddress customerAddress = InetAddress.getByName(args[8]);
	    final int customerPort = Integer.parseInt(args[9]);
	    final ResourceManager customerManager =
            new SocketResourceManager(customerAddress, customerPort);

        ResourceManager resourceManager =
            new MiddlewareResourceManager(
                    flightManager,
                    carManager,
                    roomManager,
                    customerManager);

        new MiddlewareBuilder()
            .withListenPort(port)
            .withBindAddress(bindAddress)
            .withResourceManager(resourceManager)
            .build()
            .run();

        System.out.println("RM Sockets Server is exiting. Have a nice day.");
    }

    public static class MiddlewareBuilder {
        ResourceManager resourceManager;
        int listenPort;
        InetAddress bindAddress;

        public MiddlewareBuilder() {

        }

        public MiddlewareBuilder withResourceManager(ResourceManager rm) {
            resourceManager = rm;
            return this;
        }

        public MiddlewareBuilder withListenPort(int port) {
            listenPort = port;
            return this;
        }

        public MiddlewareBuilder withBindAddress(InetAddress address) {
            bindAddress = address;
            return this;
        }

        public Middleware build() throws IOException {
            return new Middleware(listenPort, bindAddress, resourceManager);
        }
    }
}
