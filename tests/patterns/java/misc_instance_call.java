import world.Hello;

public class test {
    public static void main(String[] argv) throws Exception {
        Object m = new Hello.internalB.f(); // internalB is an internal class of class Hello, f is a staic function in the class B
        // MATCH: internal_class_instance_call
        Object m = new Hello.internalB.t(); // internalB is an internal class of class Hello, t is an instance function in the class B
    }
}
