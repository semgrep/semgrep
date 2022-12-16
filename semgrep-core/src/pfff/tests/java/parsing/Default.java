public class Foo {
  public static void main(String args[]) {
      Item item = new Item();
    }
}

class Item implements Descriptive {
}

interface Descriptive {
    default String desc() {
        return "Hello";
    }
}
