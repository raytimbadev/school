package client;

import common.ResourceManager;
import common.NoSuchTransactionException;

import java.io.*;
import java.net.InetAddress;
import java.util.*;


public class ScriptedClient {
    final ResourceManager proxy;

    public ScriptedClient(InetAddress address, int port) {
        proxy = new SocketResourceManager(address, port);
    }

    public static void main(String[] args) {
        try {
            if (args.length != 5) {
                System.out.println("Usage: MyClient <service-name> "
                        + "<service-host> <service-port> " +
                        "<loop count> " + "<transactions per second> " +
                        "<test 0(single) or test 1(multi)>");
                System.exit(-1);
            }

            final InetAddress address = InetAddress.getByName(args[0]);
            int servicePort = Integer.parseInt(args[1]);
            int loopCount = Integer.parseInt(args[2]);
            int transactionPerSecond = Integer.parseInt(args[3]);
            int test = Integer.parseInt(args[4]);

            if(test != 1 && test !=0) {
                System.out.println("test identifier must be 0 or 1");
                System.exit(-1);
            }

            ScriptedClient client = new ScriptedClient(address, servicePort);
            client.run(loopCount, transactionPerSecond,test);
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    /**
    * Runs a pre-determined set of actions in a transaction
    */
    private void run(int loopCount, int transactionPerSecond,int test) {
        int ms = 1000/transactionPerSecond;
        ArrayList<int[]> transactionTimes = new ArrayList<int[]>();

        for(int i=0; i < loopCount; i++) {
            int id = proxy.newCustomer(-1);
            int time = (int)System.currentTimeMillis();
            if(i%2==0) {
                flightTransaction(id);
            }
            else {
                multiTransaction(id);
            }
            //time now contains the transaction's run time
            time = (int)System.currentTimeMillis() - time;
            int[] array = {time, i};
            transactionTimes.add(array);
            System.out.println("Completed Transaction: "+ i + ", of:" + loopCount);
            if(false) {
                try{
                    Thread.sleep(ms-time);
                }
                catch(InterruptedException e) {
                    System.err.println(e.getMessage());
                }
            }
        }
        try {
            FileWriter writer= new FileWriter("log/log.txt", true);
            for(int i =0; i < transactionTimes.size();i++) {
                writer.write(transactionTimes.get(i)[0]+","+ transactionTimes.get(i)[1]+"\n");
            }
            writer.close();
        }
        catch(IOException e) {
            System.err.println("IOException: "+ e.getMessage());
        }
    }

    /*Flight transaction used for single RM, single client test case*/
    private void flightTransaction(int id) {
        int tid = proxy.start();

        proxy.addFlight(tid,1,1,100);
        proxy.addFlight(tid,2,1,100);
        proxy.reserveFlight(tid,id,1);
        proxy.reserveFlight(tid,id,2);
        proxy.addFlight(tid,3,1,100);
        proxy.reserveFlight(tid,id,3);

        try {
            proxy.commit(tid);
        }
        catch(NoSuchTransactionException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    /*Multi transaction used for multiple RM single client test case*/
    private void multiTransaction(int id) {
        int tid = proxy.start();

        proxy.addFlight(tid,1,1,100);
        proxy.reserveFlight(tid,id,1);
        proxy.addCars(tid,"montreal",1,100);
        proxy.reserveCar(tid,id,"montreal");
        proxy.addRooms(tid,"montreal",1,100);
        proxy.reserveRoom(tid,id,"montreal");

        try {
            proxy.commit(tid);
        }
        catch(NoSuchTransactionException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}
