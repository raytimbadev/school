package client;

import java.net.URL;
import java.net.MalformedURLException;

public class WSClient {

    MiddlewareManagerImplService service;
    
    ResourceManager proxy;
    
    public WSClient(String serviceName, String serviceHost, int servicePort) 
    throws MalformedURLException {
    
        URL wsdlLocation = new URL("http", serviceHost, servicePort, 
                "/" + serviceName + "/middleware?wsdl");
                
        service = new MiddlewareManagerImplService(wsdlLocation);
        
        proxy = (ResourceManager) service.getMiddlewareManagerImplPort();
    }

    public ResourceManager getProxy() {
        return proxy;
    }
}
