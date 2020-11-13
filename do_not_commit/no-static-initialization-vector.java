public class StaticIV {

    public StaticIV() {
        // ruleid: no-static-initialization-vector
        byte[] iv = {
            (byte) 0, (byte) 0, (byte) 0, (byte) 0,
            (byte) 0, (byte) 0, (byte) 0, (byte) 0,
            (byte) 0, (byte) 0, (byte) 0, (byte) 0,
            (byte) 0, (byte) 0, (byte) 0, (byte) 0
        };
        
        IvParameterSpec staticIvSpec = new IvParameterSpec(iv);
        
        c.init(Cipher.ENCRYPT_MODE, skeySpec, staticIvSpec, new SecureRandom());
    }
}

// ruleid: no-static-initialization-vector
public class StaticIV2 {
    byte[] iv = {
        (byte) 0, (byte) 0, (byte) 0, (byte) 0,
        (byte) 0, (byte) 0, (byte) 0, (byte) 0,
        (byte) 0, (byte) 0, (byte) 0, (byte) 0,
        (byte) 0, (byte) 0, (byte) 0, (byte) 0
    };


    public StaticIV2() {
        IvParameterSpec staticIvSpec = new IvParameterSpec(iv);
        
        c.init(Cipher.ENCRYPT_MODE, skeySpec, staticIvSpec, new SecureRandom());
    }
}

public class RandomIV {

    public RandomIV() {
        // ok
        byte[] iv = new byte[16];
        new SecureRandom().nextBytes(iv);
        
        IvParameterSpec staticIvSpec = new IvParameterSpec(iv); // IvParameterSpec initialized using its own randomizer.
        
        c.init(Cipher.ENCRYPT_MODE, skeySpec, staticIvSpec, new SecureRandom());
    }
}
