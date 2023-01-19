// what actually matters in this test is the value bound to $LIB
// in the .sgrep. It should be the full path!
// use semgrep-core ... -pvar '$X' to print the value, or -json

  
//ERROR: match
import org.bouncycastle.crypto.provider.BouncyCastleProvider;
