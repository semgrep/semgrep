
String foo()
{
  // ERROR:
  xxx();
}

String foo(String a, String b)
{
  // ERROR:
  xxx();
}

class C
{
  String foo(String a, String b)
  {
    // ERROR:
    xxx();
  }
}
