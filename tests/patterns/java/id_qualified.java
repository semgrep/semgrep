class javax {
  static class crypto {
    static class Cipher {
      public static Cipher getInstance(java.lang.String s) {
        return new Cipher();
      }
    }
  }
  //ERROR:
  javax.crypto.Cipher instance = javax.crypto.Cipher.getInstance("hi");
}
