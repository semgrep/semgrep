package verify;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;

// ruleid:insecure-hostname-verifier
public class AllHosts implements HostnameVerifier {
    public boolean verify(final String hostname, final SSLSession session) {
        return true;
    }
}

// ok
public class LocalHost implements HostnameVerifier {
    public boolean verify(final String hostname, final SSLSession session) {
        return hostname.equals("localhost");
    }
}


// cf. https://stackoverflow.com/questions/2642777/trusting-all-certificates-using-httpclient-over-https
public class InlineVerifier {
    public InlineVerifier() {
        // ruleid:insecure-hostname-verifier
        HttpsURLConnection.setDefaultHostnameVerifier(new HostnameVerifier(){
            public boolean verify(String hostname, SSLSession session) {
                return true;
            }
        });
    }
}
