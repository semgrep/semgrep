# cf. https://github.com/PyCQA/bandit/blob/d5f8fa0d89d7b11442fc6ec80ca42953974354c8/examples/httplib_https.py

import httplib
# ruleid:httpsconnection-detected
c = httplib.HTTPSConnection("example.com")

import http.client
# ruleid:httpsconnection-detected
c = http.client.HTTPSConnection("example.com")

import six
# ruleid:httpsconnection-detected
six.moves.http_client.HTTPSConnection("example.com")

# ok
raise http.client.HTTPException