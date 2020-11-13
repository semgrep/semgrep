# cf. https://github.com/PyCQA/bandit/blob/b1411bfb43795d3ffd268bef17a839dee954c2b1/examples/ssl-insecure-version.py

import ssl
from pyOpenSSL import SSL

# ruleid:weak-ssl-version
ssl.wrap_socket(ssl_version=ssl.PROTOCOL_SSLv2)
# ruleid:weak-ssl-version
SSL.Context(method=SSL.SSLv2_METHOD)
# ruleid:weak-ssl-version
SSL.Context(method=SSL.SSLv23_METHOD)

# ok
ssl.wrap_socket(ssl_version=ssl.PROTOCOL_TLSv1_2)

# ruleid:weak-ssl-version
some_other_method(ssl_version=ssl.PROTOCOL_SSLv2)
# ruleid:weak-ssl-version
some_other_method(method=SSL.SSLv2_METHOD)
# ruleid:weak-ssl-version
some_other_method(method=SSL.SSLv23_METHOD)

# ruleid:weak-ssl-version
ssl.wrap_socket(ssl_version=ssl.PROTOCOL_SSLv3)
# ruleid:weak-ssl-version
ssl.wrap_socket(ssl_version=ssl.PROTOCOL_TLSv1)
# ruleid:weak-ssl-version
SSL.Context(method=SSL.SSLv3_METHOD)
# ruleid:weak-ssl-version
SSL.Context(method=SSL.TLSv1_METHOD)

# ruleid:weak-ssl-version
some_other_method(ssl_version=ssl.PROTOCOL_SSLv3)
# ruleid:weak-ssl-version
some_other_method(ssl_version=ssl.PROTOCOL_TLSv1)
# ruleid:weak-ssl-version
some_other_method(method=SSL.SSLv3_METHOD)
# ruleid:weak-ssl-version
some_other_method(method=SSL.TLSv1_METHOD)

ssl.wrap_socket()

# ruleid:weak-ssl-version
def open_ssl_socket(version=ssl.PROTOCOL_SSLv2):
    pass

# ruleid:weak-ssl-version
def open_ssl_socket(version=SSL.SSLv2_METHOD):
    pass

# ruleid:weak-ssl-version
def open_ssl_socket(version=SSL.SSLv23_METHOD):
    pass

# ruleid:weak-ssl-version
def open_ssl_socket(version=SSL.TLSv1_1_METHOD):
    pass