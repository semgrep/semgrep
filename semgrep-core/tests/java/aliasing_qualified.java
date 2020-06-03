package testcode.crypto;

import javax.crypto.Cipher;

class Foo {
    //ERROR:
    Cipher c = Cipher.getInstance("DES/ECB/PKCS5Padding");
}
