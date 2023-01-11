import java.util.*;

public class Foo {
	public static void main(String[] args) {
		List<String> strings = Arrays.asList("a", "b");

        long count = strings.stream()
                            .filter(String::isEmpty)
                            .count();
	}
}
