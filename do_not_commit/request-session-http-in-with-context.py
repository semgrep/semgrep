import requests

def test1(): 
    with requests.Session() as session:
        # ruleid: request-session-http-in-with-context
        session.get("http://example.com")

def test1_ok():
    with requests.Session() as session:
        # ok: request-session-http-in-with-context
        session.get("https://example.com")

def test2():
    with requests.Session() as session:
        url = "http://example.com"
        # ruleid: request-session-http-in-with-context
        session.post(url)

def test2_ok():
    with requests.Session() as session:
        url = "https://example.com"
        # ok: request-session-http-in-with-context
        session.post(url)

def test3():
    url = "http://example.com"
    with requests.Session() as session:
        # ruleid: request-session-http-in-with-context
        session.post(url)

def test3_ok():
    url = "https://example.com"
    with requests.Session() as session:
        # ok: request-session-http-in-with-context
        session.post(url)