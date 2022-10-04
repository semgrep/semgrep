public class Test {
    public void test() {
        //ruleid:insecure-crypto-usage
        MessageDigest.sha1("MD2");
    }
}
