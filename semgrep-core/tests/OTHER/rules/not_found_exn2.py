from cryptography.hazmat.primitives.asymmetric import rsa

def rsa_priv(foo, key_size='4096'):
    size = int(key_size)

    key = rsa.generate_private_key(public_exponent=65537, key_size=size,
                                   backend=default_backend())
