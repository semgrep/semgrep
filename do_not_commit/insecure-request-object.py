from urllib.request import Request

def test1():
    # ruleid: insecure-request-object
    Request("http://example.com")

def test1_ok():
    # ok: insecure-request-object
    Request("https://example.com")

def test2():
    # ruleid: insecure-request-object
    url = "http://example.com"
    Request(url)

def test2_ok():
    # ok: insecure-request-object
    url = "https://example.com"
    Request(url)

# ruleid: insecure-request-object
def test3(url = "http://example.com"):
    Request(url)
    
# ok: insecure-request-object
def test3_ok(url = "https://example.com"):
    Request(url)
    