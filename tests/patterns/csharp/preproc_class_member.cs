// ERROR:
class C {
  void Other() { }
#if DEBUG
  public void Log() { }
#endif
}

class D {
  void Other() { }
}
