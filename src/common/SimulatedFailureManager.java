package common; 

import java.util.concurrent.ScheduledExecutorService; 
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;  
import common.SimulatedFailure;

public class SimulatedFailureManager {
	private static SimulatedFailureManager instance; 
	private final ScheduledExecutorService crashService; 
	private SimulatedFailure failure;

	private SimulatedFailureManager(){
		crashService = Executors.newSingleThreadScheduledExecutor(); 
		failure = null; 
	}

	public static SimulatedFailureManager getInstance() { 
		if(instance == null) {
			instance = new SimulatedFailureManager();
		}
		return instance; 
	}

	public void scheduleCrash(int s) {
		crashService.schedule(new Runnable(){public void run() { System.exit(1);}},
							  s, TimeUnit.SECONDS); 
	}

	public SimulatedFailure getFailure() {
		return failure; 
	}

	public void setFailure(SimulatedFailure failure) {
		this.failure = failure; 
	}

	public void crash() {
		Trace.info("Crashing middleware"); 
		System.exit(1);
	}
}