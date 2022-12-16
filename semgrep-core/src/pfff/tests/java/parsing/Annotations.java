
class Annotations extends Base {
  @Override
  void foo(String args[])
  {
    System.out.println("Hello World from Annotations!");
  }

  public static void main(String args[])
  {
    (new Annotations()).foo(args);
  }
}
