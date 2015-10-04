package middleware;

import java.io.File;
import org.apache.catalina.LifecycleException;
import org.apache.catalina.startup.Tomcat;
import java.net.URL;
import java.net.MalformedURLException;

public class Main {
    public static void main(String[] args) 
    throws Exception {
        System.out.println("This is the middleware!");

        if(args.length != 10) {
            System.out.println(
                    "Bad command line arguments."
            );
            System.exit(1);
        }

        String middlewareServiceName = args[0];
        String rmServiceName = args[1];
        int servicePort = Integer.parseInt(args[2]);
        String deployDir = args[3];

        String flightHost = args[4];
        int flightPort = Integer.parseInt(args[5]);

        String carHost = args[6];
        int carPort = Integer.parseInt(args[7]);

        String roomHost = args[8];
        int roomPort = Integer.parseInt(args[9]);

        URL flightWsdlLocation = new URL(
                "http",
                flightHost,
                flightPort, 
                "/" + rmServiceName + "/service?wsdl"
        );
                
        URL carWsdlLocation = new URL(
                "http",
                carHost,
                carPort,
                "/" + rmServiceName + "/service?wsdl"
        );

        URL roomWsdlLocation = new URL(
                "http",
                roomHost,
                roomPort,
                "/" + rmServiceName + "/service?wsdl"
        );

        ResourceManagerImplService flightService = new ResourceManagerImplService(
                flightWsdlLocation
        );

        ResourceManager flightManager = flightService.getResourceManagerImplPort();

        ResourceManagerImplService carService = new ResourceManagerImplService(
                carWsdlLocation
        );

        ResourceManager carManager = carService.getResourceManagerImplPort();

        ResourceManagerImplService roomService = new ResourceManagerImplService(
                roomWsdlLocation
        );

        ResourceManager roomManager = roomService.getResourceManagerImplPort();

        ManagerManager.initialize(flightManager, carManager, roomManager);

        Tomcat tomcat = new Tomcat();
        tomcat.setPort(servicePort);
        tomcat.setBaseDir(deployDir);

        tomcat.getHost().setAppBase(deployDir);
        tomcat.getHost().setDeployOnStartup(true);
        tomcat.getHost().setAutoDeploy(true);

        tomcat.addWebapp(
                "/" + middlewareServiceName,
                new File(deployDir + "/" + middlewareServiceName).getAbsolutePath()
        );

        System.out.println(
                new File(deployDir + "/" + middlewareServiceName).getAbsolutePath()
        );

        tomcat.start();
        tomcat.getServer().await();
    }
}
