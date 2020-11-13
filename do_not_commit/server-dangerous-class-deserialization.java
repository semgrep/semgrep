// cf. https://mogwailabs.de/blog/2019/03/attacking-java-rmi-services-after-jep-290/

package de.mogwailabs.BSidesRMIService;

import java.rmi.Naming;
import java.rmi.registry.LocateRegistry;

import java.rmi.Remote;
import java.rmi.RemoteException;

// ruleid:server-dangerous-class-deserialization
public interface IBSidesService extends Remote {
   boolean registerTicket(String ticketID) throws RemoteException;
   void vistTalk(String talkname) throws RemoteException;
   void poke(Attendee attende) throws RemoteException;
}

// ok:server-dangerous-class-deserialization
public interface IBSidesServiceOK extends Remote {
   boolean registerTicket(long ticketID) throws RemoteException;
   void vistTalk(long talkID) throws RemoteException;
   void poke(int attende) throws RemoteException;
}

public class Attendee {
    public int id;
    public String handle;
}

public class BSidesServer {
	public static void main(String[] args) {
		try {
			// Create new RMI registry to which we can register
			LocateRegistry.createRegistry(1099);

			// Make our BSides Server object 
			// available under the name "bsides"
			Naming.bind("bsides", new BSidesServiceServerImpl()); 
			System.out.println("BSides RMI server is ready");
		
		} catch (Exception e) {
			// In case of an error, print the stacktrace 
			// and bail out
			e.printStackTrace();
		} 
	}
}
