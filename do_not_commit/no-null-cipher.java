import java.lang.Runtime;

class Cls {

    public Cls() {
        System.out.println("Hello");
    }
    
    public byte[] test1(String plainText) {
        // ruleid: no-null-cipher
        Cipher doNothingCihper = new NullCipher();
        //The ciphertext produced will be identical to the plaintext.
        byte[] cipherText = doNothingCihper.doFinal(plainText);
        return cipherText;
    }

    public void test2(String plainText) {
        // ok
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        byte[] cipherText = cipher.doFinal(plainText);
        return cipherText;
    }
}
