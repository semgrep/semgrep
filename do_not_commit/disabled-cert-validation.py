
import requests as req
import requests

some_url = "https://example.com"

# ok
r = req.get(some_url, stream=True)
# ok
r = requests.post(some_url, stream=True)

# ruleid:disabled-cert-validation
r = req.get(some_url, stream=True, verify=False)
# ruleid:disabled-cert-validation
r = requests.post(some_url, stream=True, verify=False)
# ruleid:disabled-cert-validation
r = requests.post(some_url, verify=False, stream=True)