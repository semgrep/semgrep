class A {
    int x;
}

class HelloWorld {
    public static void main(String[] args) {
      A a = new A();
      A b = new A();
      A c = new A();
      a = b;
      b = a;
      c = b;
    }
}
