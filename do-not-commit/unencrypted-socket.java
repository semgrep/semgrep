package testcode.crypto;

import javax.net.ssl.SSLServerSocketFactory;
import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.net.ServerSocket;

public class UnencryptedSocket {

    static void sslSocket() throws IOException {
        // ok
        Socket soc = SSLSocketFactory.getDefault().createSocket("www.google.com", 443);
        doGetRequest(soc);
    }

    static void plainSocket() throws IOException {
        // ruleid: unencrypted-socket
        Socket soc = new Socket("www.google.com", 80);
        doGetRequest(soc);
    }

    static void otherConstructors() throws IOException {
        // ruleid: unencrypted-socket
        Socket soc1 = new Socket("www.google.com", 80, true);
        doGetRequest(soc1);
        byte[] address = {127, 0, 0, 1};
        // ruleid: unencrypted-socket
        Socket soc2 = new Socket("www.google.com", 80, InetAddress.getByAddress(address), 13337);
        doGetRequest(soc2);
        byte[] remoteAddress = {74, 125, (byte) 226, (byte) 193};
        // ruleid: unencrypted-socket
        Socket soc3 = new Socket(InetAddress.getByAddress(remoteAddress), 80);
        doGetRequest(soc2);
    }

    static void doGetRequest(Socket soc) throws IOException {
        System.out.println("");
        soc.close();
    }
}

public class UnencryptedServerSocket {

    static void sslServerSocket() throws IOException {
        // ok
        ServerSocket ssoc = SSLServerSocketFactory.getDefault().createServerSocket(1234);
        ssoc.close();
    }

    static void plainServerSocket() throws IOException {
        // ruleid: unencrypted-socket
        ServerSocket ssoc = new ServerSocket(1234);
        ssoc.close();
    }

    static void otherConstructors() throws IOException {
        // ruleid: unencrypted-socket
        ServerSocket ssoc1 = new ServerSocket();
        ssoc1.close();
        // ruleid: unencrypted-socket
        ServerSocket ssoc2 = new ServerSocket(1234, 10);
        ssoc2.close();
        byte[] address = {127, 0, 0, 1};
        // ruleid: unencrypted-socket
        ServerSocket ssoc3 = new ServerSocket(1234, 10, InetAddress.getByAddress(address));
        ssoc3.close();
    }

}
