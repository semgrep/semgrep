public class Foo {
    public static void main(String[] args) {
    }

    @FunctionalInterface
    public interface Interface1 {

    	void method1(String str);

    	default void log(String str){
    		System.out.println("Logging "+str);
    	}

    }
}
