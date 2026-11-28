public static class Ext {
  // ERROR:
  extension(string s) {
    public int Twice => s.Length * 2;
  }

  // ERROR:
  extension<T>(List<T> list) {
    public void Clear2() => list.Clear();
  }
}

public static class NotExt {
  // An ordinary method is not an extension block.
  public static void extension(string s) { }

  // Nor is a call to a method named `extension`.
  static void Caller(string s) {
    extension(s);
  }
}
