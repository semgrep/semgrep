public class Foo {
    public static void main(String[] args) {
    }

    @FunctionalInterface
    public interface Interface1 {

    	void method1(String str);

    	static void print(String str){
    		System.out.println("Printing "+str);
    	}

    }
}
