import java.lang.Runtime;

class Cls {

    public Cls(String input) {
        Runtime r = Runtime.getRuntime();
        // ruleid: command-injection-formatted-runtime-call
        r.exec("/bin/sh -c some_tool" + input);
    }
    
    public void test1(String input) {
        Runtime r = Runtime.getRuntime();
        // ruleid: command-injection-formatted-runtime-call
        r.loadLibrary(String.format("%s.dll", input));
    }

    public void test2(String input) {
        Runtime r = Runtime.getRuntime();
        // ok
        r.exec("echo 'blah'");
    }
}
