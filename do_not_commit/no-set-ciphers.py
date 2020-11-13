import ssl

context = ssl.create_default_context()

# cf. https://stackoverflow.com/questions/49774366/how-to-set-ciphers-in-ssl-python-socket
cipher = 'DHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA:ECDHE-ECDSA-AES128-GCM-SHA256'
# ruleid: no-set-ciphers
context.set_ciphers(cipher)

# ok: no-set-ciphers
print(context.get_ciphers())