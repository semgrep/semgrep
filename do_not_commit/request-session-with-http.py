import requests

def test1(): 
    session = requests.Session()
    # ruleid: request-session-with-http
    session.get("http://example.com")

def test1_ok():
    session = requests.Session()
    # ok: request-session-with-http
    session.get("https://example.com")

def test2():
    session = requests.Session()
    # ruleid: request-session-with-http
    url = "http://example.com"
    session.post(url)

def test2_ok():
    session = requests.Session()
    # ok: request-session-with-http
    url = "https://example.com"
    session.post(url)

# ruleid: request-session-with-http
def test3(url = "http://example.com"):
    session = requests.Session()
    session.delete(url)

# ok: request-session-with-http
def test3_ok(url = "https://example.com"):
    session = requests.Session()
    session.delete(url)

# ruleid: request-session-with-http
def test4(url = "http://example.com"):
    session = requests.Session()
    session.request("HEAD", url, timeout=30)

# ok: request-session-with-http
def test4_ok(url = "https://example.com"):
    session = requests.Session()
    session.request("HEAD", url, timeout=30)
    