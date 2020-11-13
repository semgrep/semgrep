import java.security.MessageDigest;
import org.apache.commons.codec.digest.DigestUtils;

public class WeakHashes {
  public byte[] sha1(String password) {
      // ruleid: use-of-sha1
      MessageDigest sha1Digest = MessageDigest.getInstance("SHA1");
      sha1Digest.update(password.getBytes());
      byte[] hashValue = sha1Digest.digest();
      return hashValue;
  }
  public byte[] sha1_digestutil(String password) {
    // ruleid: use-of-sha1
    byte[] hashValue = DigestUtils.getSha1Digest().digest(password.getBytes());
    return hashValue;
  }

  public byte[] md5(String password) {
    // ruleid: use-of-md5
    MessageDigest md5Digest = MessageDigest.getInstance("MD5");
    md5Digest.update(password.getBytes());
    byte[] hashValue = md5Digest.digest();
    return hashValue;
  }

  public byte[] md5_digestutil(String password) {
    // ruleid: use-of-md5
    byte[] hashValue = DigestUtils.getMd5Digest().digest(password.getBytes());
    return hashValue;
  }
}
