//ERROR: match
import org.bouncycastle.crypto.provider.BouncyCastleProvider; // would expect this to also match

//TODO have a pattern like import $LIB... new $LIB()
class Foo {
  void main() {
    return new org.bouncycastle.crypto.provider.BouncyCastleProvider();
  }
}
