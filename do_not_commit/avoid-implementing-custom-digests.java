// ruleid: avoid-implementing-custom-digests
public class MyProprietaryMessageDigest extends MessageDigest {

    @Override
    protected byte[] engineDigest() {
        return "";
    }
}

// ok
public class NotMessageDigest {
    public NotMessageDigest() {
        System.out.println("");
    }
}