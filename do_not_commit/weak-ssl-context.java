import java.lang.Runtime;

class Cls {

    public Cls() {
        System.out.println("Hello");
    }
    
    public void test1() {
        // ruleid: weak-ssl-context
        SSLContext ctx = SSLContext.getInstance("SSL");
    }

    public void test2() {
        // ruleid: weak-ssl-context
        SSLContext ctx = SSLContext.getInstance("TLS1.0");
    }

    public void test3() {
        // ruleid: weak-ssl-context
        SSLContext ctx = SSLContext.getInstance("TLS1.1");
    }

    public void test4() {
        // ok
        SSLContext ctx = SSLContext.getInstance("TLS1.2");
    }
    
    public void test5() {
        // ok
        SSLContext ctx = SSLContext.getInstance("TLS1.3");
    }

    public String getSslContext() {
        return "Anything";
    }
    
    public void test5() {
        // ok
        SSLContext ctx = SSLContext.getInstance(getSslContext());
    }
}
