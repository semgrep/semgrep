import java.security.KeyPairGenerator;

public class WeakRSA {
  
  static void rsaWeak() {
    // ruleid: use-of-weak-rsa-key
    KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA");
    keyGen.initialize(512);
  } 
  
  static void rsaOK() {
    KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA");
    keyGen.initialize(2048);
  }
}