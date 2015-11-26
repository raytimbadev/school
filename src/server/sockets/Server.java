package server.sockets;

import common.ResourceManager;
import common.Trace;
import server.CustomerResourceManager;
import server.ItemResourceManager;

import common.sockets.RequestContext;
import common.sockets.RequestHandler;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

public class Server implements Runnable {
    public static final int BACKLOG_MULTIPLIER = 5;
    public static final int DEFAULT_REQUEST_TTL = 90;

    public final int THREAD_COUNT;

    private int requestNumber;

    final ExecutorService executorService;
    final ServerSocket serverSocket;
    final ResourceManager resourceManager;
    final ScheduledExecutorService ttlChecker;

    final int requestTtl;

    public Server(int port, InetAddress bindAddress, ResourceManager manager)
    throws IOException {
        resourceManager = manager;
        THREAD_COUNT = Runtime.getRuntime().availableProcessors();
        executorService = Executors.newFixedThreadPool(THREAD_COUNT);

        serverSocket = new ServerSocket(
                port,
                BACKLOG_MULTIPLIER * THREAD_COUNT,
                bindAddress
        );

        ttlChecker = Executors.newSingleThreadScheduledExecutor();
        requestTtl = DEFAULT_REQUEST_TTL;

        Trace.info(String.format(
                    "Launched RM capable of %d concurrent connections with " +
                    "%d seconds request timeout.",
                    THREAD_COUNT));
    }

    public Server(
            int port, 
            InetAddress bindAddress, 
            ResourceManager manager,
            int threadCount) 
    throws IOException {
        resourceManager = manager;
        THREAD_COUNT = threadCount;
        executorService = Executors.newFixedThreadPool(THREAD_COUNT);

        serverSocket = new ServerSocket(
                port,
                BACKLOG_MULTIPLIER * THREAD_COUNT,
                bindAddress);

        ttlChecker = Executors.newSingleThreadScheduledExecutor();
        requestTtl = DEFAULT_REQUEST_TTL;

        Trace.info(
                String.format(
                    "Launched RM capable of %d concurrent connections with " +
                    "%d seconds request timeout.",
                    THREAD_COUNT,
                    requestTtl
                )
        );
    }

    @Override
    public void run() {
        boolean running = true;
        requestNumber = 1;

        while(running) {
            try {
                final Socket clientSocket = serverSocket.accept();
                final RequestContext ctx =
                    new RequestContext.RequestContextBuilder()
                        .withResourceManager(resourceManager)
                        .withClientSocket(clientSocket)
                        .withRequestNumber(requestNumber)
                        .build();
                final RequestHandler handler = new RequestHandler(ctx);

                Trace.info(
                        String.format(
                            "Submitted request %d.",
                            requestNumber
                        )
                );

                final Future future = executorService.submit(handler);

                ttlChecker.schedule(
                        new RequestCanceller(future, requestNumber),
                        requestTtl,
                        TimeUnit.SECONDS
                );
            }
            catch(IOException e) {
                e.printStackTrace();
            }

            requestNumber++;
        }
    }

    public static void main(String[] args)
    throws Exception {
        String address = args[0];
        int port = Integer.parseInt(args[1]);
        String serverType = args[2];
        int threadCount = Integer.parseInt(args[3]);

        final String serverName = String.format(
                "%sdb%d",
                serverType,
                port
        );

        // TODO read a file with this configuration
        ResourceManager resourceManager = null;
        if(serverType.equals("customer")) {
            resourceManager = new CustomerResourceManager(serverName);
        }
        else if(serverType.equals("item")) {
            resourceManager = new ItemResourceManager(serverName);
        }
        else
            throw new RuntimeException(
                    "Please give the server a type: 'customer' or 'item'."
            );

        Server server = new ServerBuilder()
            .withListenPort(port)
            .withBindAddress(InetAddress.getByName(address))
            .withResourceManager(resourceManager)
            .withThreadCount(threadCount)
            .build();

        server.run();

        System.out.println("RM Sockets Server is exiting. Have a nice day.");
    }

    public static class ServerBuilder {
        ResourceManager resourceManager;
        int listenPort;
        InetAddress bindAddress;
        int threadCount;

        public ServerBuilder() {

        }

        public ServerBuilder withResourceManager(ResourceManager rm) {
            resourceManager = rm;
            return this;
        }

        public ServerBuilder withListenPort(int port) {
            listenPort = port;
            return this;
        }

        public ServerBuilder withBindAddress(InetAddress address) {
            bindAddress = address;
            return this;
        }

        public ServerBuilder withThreadCount(int count) {
            threadCount = count;
            return this;
        }

        public Server build() throws IOException {
            return new Server(
                    listenPort,
                    bindAddress,
                    resourceManager,
                    threadCount);
        }
    }

    private static class RequestCanceller implements Runnable {
        private final Future future;
        private final int requestNumber;

        public RequestCanceller(Future future, int requestNumber) {
            this.future = future; 
            this.requestNumber = requestNumber;
        }

        @Override
        public void run() {
            if(future.cancel(true))
                Trace.warn(
                        String.format(
                            "Cancelled request %d.",
                            requestNumber
                        )
                );
        }
    }
}
