# cf. https://github.com/PyCQA/bandit/blob/b1411bfb43795d3ffd268bef17a839dee954c2b1/examples/weak_cryptographic_key_sizes.py

import os
from Crypto.PublicKey import DSA as pycrypto_dsa
from Cryptodome.PublicKey import DSA as pycryptodomex_dsa

# ok
pycrypto_dsa.generate(bits=2048)
# ok
pycryptodomex_dsa.generate(bits=2048)

# ok
pycrypto_dsa.generate(4096)
# ok
pycryptodomex_dsa.generate(4096)

# ruleid:insufficient-dsa-key-size
pycrypto_dsa.generate(bits=1024)
# ruleid:insufficient-dsa-key-size
pycryptodomex_dsa.generate(bits=1024)

# ruleid:insufficient-dsa-key-size
pycrypto_dsa.generate(512)
# ruleid:insufficient-dsa-key-size
pycryptodomex_dsa.generate(512)

pycrypto_dsa.generate(os.getenv("KEY_SIZE"))
pycryptodomex_dsa.generate(os.getenv("KEY_SIZE"))