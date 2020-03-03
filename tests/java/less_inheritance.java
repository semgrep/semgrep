// ERROR: match
public class A {
  public static foo() {
    return 1;
  }
}

// ERROR: match
public class B extends A {
  public static bar() {
    return 1;
  }
}

public class C extends B{
  public static foo() {
    return 11;
  }
}
