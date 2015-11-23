package client;

import common.ResourceManager;
import common.NoSuchTransactionException;
import common.InvalidTransactionException;
import common.Trace;

import lockmanager.InvalidLockException;

import java.io.*;
import java.net.InetAddress;
import java.util.*;


public class ScriptedClient {
    final ResourceManager proxy;
    String[] cities = {
"Afghanistan",
"Albania",
"Algeria",
"Andorra",
"Angola",
"Antigua", "Deps",
"Argentina",
"Armenia",
"Australia",
"Austria",
"Azerbaijan",
"Bahamas",
"Bahrain",
"Bangladesh",
"Barbados",
"Belarus",
"Belgium",
"Belize",
"Benin",
"Bhutan",
"Bolivia",
"Bosnia Herzegovina",
"Botswana",
"Brazil",
"Brunei",
"Bulgaria",
"Burkina",
"Burundi",
"Cambodia",
"Cameroon",
"Canada",
"Cape Verde",
"Central African Rep",
"Chad",
"Chile",
"China",
"Colombia",
"Comoros",
"Congo"};

    public ScriptedClient(InetAddress address, int port) {
        proxy = new SocketResourceManager(address, port);
    }

    public static void main(String[] args) {
        try {
            if (args.length != 6) {
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
            String filename = args[5]; 

            if(test != 1 && test !=0) {
                System.out.println("test identifier must be 0 or 1");
                System.exit(-1);
            }

            ScriptedClient client = new ScriptedClient(address, servicePort);
            client.run(loopCount, transactionPerSecond,test,filename);
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    /**
    * Runs a pre-determined set of actions in a transaction
    */
    private void run(int loopCount, int transactionPerSecond,int test,String filename) {
        int ms = 1000/transactionPerSecond;
        ArrayList<int[]> transactionTimes = new ArrayList<int[]>();

        for(int i=0; i < loopCount; i++) {
            int id = proxy.newCustomer(-1);
            int time = (int)System.currentTimeMillis();
            boolean failed = false;
            try {
                if(false) {
                    flightTransaction(id);
                }
                else if(false) {
                    multiTransaction(id);
                } else {
                    duoTransaction(id); 
                }
            }
            catch(InvalidLockException e) {
                Trace.warn(String.format("%d: %s", i, e.getMessage()));
                failed = true;
            }
            catch(InvalidTransactionException e) {
                Trace.warn(String.format("%d: %s", i, e.getMessage()));
                failed = true;
            }
            catch(Exception e) {
                Trace.warn(String.format("%d: %s", i, e.getMessage()));
                failed = true;
            }
            //time now contains the transaction's run time
            time = (int)System.currentTimeMillis() - time;
            int[] array = {time, i};
            transactionTimes.add(array);
            System.out.println("Completed Transaction: "+ i + ", of:" + loopCount);
            if(test==1) {
                try{
                    if(ms-time>0){
                        Thread.sleep(ms-time);
                    }
                }
                catch(InterruptedException e) {
                    System.err.println(e.getMessage());
                }
            }
        }
        try { 
            FileWriter writer= new FileWriter(filename, true);
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
        int r1 = (int)(Math.random()*100000); //constants yay
        int r2 = (int)(Math.random()*100000); 
        int r3 = (int)(Math.random()*100000); 
        proxy.addFlight(tid,r1,1,100);
        proxy.addFlight(tid,r2,1,100);
        proxy.reserveFlight(tid,id,r1);
        proxy.reserveFlight(tid,id,r2);
        proxy.addFlight(tid,r3,1,100);
        proxy.reserveFlight(tid,id,r3);

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
        int r1 = (int)(Math.random()*100000); 
        String cn1 = cities[(int)(Math.random()*(cities.length-1))];
        String cn2 = cities[(int)(Math.random()*(cities.length-1))]; 
        proxy.addFlight(tid,r1,1,100);
        proxy.reserveFlight(tid,id,r1);
        proxy.addCars(tid,cn1,1,100);
        proxy.reserveCar(tid,id,cn1);
        proxy.addRooms(tid,cn2,1,100);
        proxy.reserveRoom(tid,id,cn2);

        try {
            proxy.commit(tid);
        }
        catch(NoSuchTransactionException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void duoTransaction(int id) {
        int r1 = (int)(Math.random()*100000); 
        String cn1 = cities[(int)(Math.random()*(cities.length-1))];
        int r2 =(int)(Math.random()*100000);  

        int tid = proxy.start();
        proxy.addFlight(tid,r1,1,100);
        proxy.addFlight(tid,r2,1,100);
        proxy.addCars(tid,cn1,1,100);
        proxy.reserveFlight(tid,id,r1);
        proxy.reserveFlight(tid,id,r2);
        proxy.reserveCar(tid,id,cn1); 
        try {
            proxy.commit(tid);
        }
        catch(NoSuchTransactionException e) {
            e.printStackTrace();
            System.exit(1);
        }

    }
}
