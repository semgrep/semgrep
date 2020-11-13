class RSAPadding {
  public void rsaNoPadding() {
    // ruleid: rsa-no-padding
    Cipher.getInstance("RSA/NONE/NoPadding");
  }
  
  public void rsaNoPadding() {
    // ok
    Cipher.getInstance("RSA/ECB/OAEPWithMD5AndMGF1Padding");
  }
}

