import ssl
import httplib.client

# ok
context = ssl.create_default_context()
conn = httplib.client.HTTPSConnection("123.123.21.21", context=context)

# ruleid:unverified-ssl-context
context = ssl._create_unverified_context()
conn = httplib.client.HTTPSConnection("123.123.21.21", context=context)

# ruleid:unverified-ssl-context
conn = httplib.client.HTTPSConnection("123.123.21.21", context=ssl._create_unverified_context())