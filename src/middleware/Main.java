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

        if(args.length != 4) {
            System.out.println(
                    "Bad command line arguments."
            );
            System.exit(1);
        }

        String middlewareServiceName = args[0];
        String rmServiceName = args[1];
        int servicePort = Integer.parseInt(args[2]);
        String deployDir = args[3];

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
