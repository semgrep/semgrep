import requests

def test1():
    # ruleid: request-with-http
    requests.get("http://example.com")

def test1_ok():
    # ok: request-with-http
    requests.get("https://example.com")

def test2():
    # ruleid: request-with-http
    url = "http://example.com"
    requests.post(url)

def test2_ok():
    # ok: request-with-http
    url = "https://example.com"
    requests.post(url)

# ruleid: request-with-http
def test3(url = "http://example.com"):
    requests.delete(url)
    
# ok: request-with-http
def test3_ok(url = "https://example.com"):
    requests.delete(url)

# ruleid: request-with-http
def test4(url = "http://example.com"):
    requests.request("HEAD", url, timeout=30)
    
# ok: request-with-http
def test4_ok(url = "https://example.com"):
    requests.request("HEAD", url, timeout=30)

# ruleid: request-with-http
def test5(url = "http://example.com"):
    requests.Request("HEAD", url, timeout=30)
    
# ok: request-with-http
def test5_ok(url = "https://example.com"):
    requests.Request("HEAD", url, timeout=30)