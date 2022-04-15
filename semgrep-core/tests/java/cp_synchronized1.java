class Test {
    void f(String x) {
        String y = x;
        String z = "foo";
        synchronized (this) {
            //ERROR:
            test(z);
            y = z;
            z = x;
        }
        //ERROR:
        test(y);
        //OK:
        test(z);
    }
}
