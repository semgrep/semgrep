// ERROR:
String foo()
{
  xxx();
}

// ERROR:
String foo(String a, String b)
{
  xxx();
}

class C
{
  // ERROR:
  String foo(String a, String b)
  {
    xxx();
  }
}
