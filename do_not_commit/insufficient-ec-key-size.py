import os
from cryptography.hazmat import backends
from cryptography.hazmat.primitives.asymmetric import ec

# ok
ec.generate_private_key(curve=ec.SECP256K1,
                         backend=backends.default_backend())

# ok
ec.generate_private_key(ec.SECP256K1,
                         backends.default_backend())

# ok
ec.generate_private_key(curve=os.getenv("EC_CURVE"),
                         backend=backends.default_backend())

# ruleid: insufficient-ec-key-size
ec.generate_private_key(curve=ec.SECP192R1,
                         backend=backends.default_backend())

# ruleid: insufficient-ec-key-size
ec.generate_private_key(ec.SECT163K1,
                         backends.default_backend())