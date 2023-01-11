import java.util.*;

public class Foo {
  public static void main(String args[])
    {
        ArrayList<Integer> arrL = new ArrayList<Integer>();
        arrL.add(1);

        // without parentheses
        arrL.forEach(n -> System.out.println(n));
    }
}
