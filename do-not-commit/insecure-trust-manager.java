package Trust;

import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import javax.net.ssl.X509TrustManager;
import javax.net.ssl.X509ExtendedTrustManager;

//cf. https://find-sec-bugs.github.io/bugs.htm#WEAK_TRUST_MANAGER
public class TrustAllManager implements X509TrustManager {

    // ruleid:insecure-trust-manager
    @Override
    public void checkClientTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {
        //Trust any client connecting (no certificate validation)
    }

    // ruleid:insecure-trust-manager
    @Override
    public void checkServerTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {
        //Trust any remote server (no certificate validation)
    }

    // ruleid:insecure-trust-manager
    @Override
    public X509Certificate[] getAcceptedIssuers() {
        return null;
    }
}

public class GoodTrustManager implements X509TrustManager {

    protected KeyStore loadKeyStore() {
        KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
        return ks;
    }

    // ok:insecure-trust-manager
    @Override
    public void checkClientTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {
        KeyStore ks = loadKeyStore();
        TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
        tmf.init(ks);
        tmf.getTrustManagers[0].checkClientTrusted(x509Certificates, s);
    }

    // ok:insecure-trust-manager
    @Override
    public void checkServerTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {
        KeyStore ks = loadKeyStore();
        TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
        tmf.init(ks);
        tmf.getTrustManagers[0].checkClientTrusted(x509Certificates, s);
    }

    // ok:insecure-trust-manager
    @Override
    public X509Certificate[] getAcceptedIssuers() {
        return loadKeyStore().getCertificate("alias");
    }
}

public final class TMClass {

    private static final X509TrustManager TM = new X509TrustManager() {
        // ruleid:insecure-trust-manager
        @Override
        public void checkClientTrusted(final X509Certificate[] chain, final String authType)
                throws CertificateException {
        }

        // ruleid:insecure-trust-manager
        @Override
        public void checkServerTrusted(final X509Certificate[] chain, final String authType)
                throws CertificateException {
        }

        // ruleid:insecure-trust-manager
        @Override
        public X509Certificate[] getAcceptedIssuers() {
            return null;
        }
    };
}

public final class TMEClass {
        TrustManager[] trustAllCerts = new TrustManager[]{new X509ExtendedTrustManager() {
        // ruleid:insecure-trust-manager
        @Override
        public X509Certificate[] getAcceptedIssuers() {
            return null;
        }

        // ruleid:insecure-trust-manager
        @Override
        public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        }

        // ruleid:insecure-trust-manager
        @Override
        public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        }

        // ruleid:insecure-trust-manager
        @Override
        public void checkClientTrusted(X509Certificate[] chain, String authType, Socket socket) throws CertificateException {
        }

        // ruleid:insecure-trust-manager
        @Override
        public void checkClientTrusted(X509Certificate[] chain, String authType, SSLEngine engine) throws CertificateException {
        }

        // ruleid:insecure-trust-manager
        @Override
        public void checkServerTrusted(X509Certificate[] chain, String authType, Socket socket) throws CertificateException {
        }

        // ruleid:insecure-trust-manager
        @Override
        public void checkServerTrusted(X509Certificate[] chain, String authType, SSLEngine engine) throws CertificateException {
        }
    }};
}
