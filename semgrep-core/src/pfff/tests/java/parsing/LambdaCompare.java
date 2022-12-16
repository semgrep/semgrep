import java.util.*;

class Dog {
    String name;
}

public class Foo {
  public static void main(String args[]) {
        ArrayList<Dog> dogs = new ArrayList<Dog>();

        Collections.sort(dogs, (d1, d2) -> d1.name.compareTo(d2.name));
    }
}
