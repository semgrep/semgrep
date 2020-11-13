from urllib.request import URLopener

def test1():
    od = URLopener()
    # ruleid: insecure-urlopener-retrieve
    od.retrieve("http://example.com")

def test1_ok():
    od = URLopener()
    # ok: insecure-urlopener-retrieve
    od.retrieve("https://example.com")

def test2():
    od = URLopener()
    # ruleid: insecure-urlopener-retrieve
    url = "http://example.com"
    od.retrieve(url)

def test2_ok():
    od = URLopener()
    # ok: insecure-urlopener-retrieve
    url = "https://example.com"
    od.retrieve(url)

def test3():
    # ruleid: insecure-urlopener-retrieve
    URLopener().retrieve("http://example.com")

def test3_ok():
    # ok: insecure-urlopener-retrieve
    URLopener().retrieve("https://example.com")

def test4():
    # ruleid: insecure-urlopener-retrieve
    url = "http://example.com"
    URLopener().retrieve(url)

def test4_ok():
    # ok: insecure-urlopener-retrieve
    url = "https://example.com"
    URLopener().retrieve(url)

def test5(url = "http://example.com"):
    # ruleid: insecure-urlopener-retrieve
    URLopener().retrieve(url)
    
def test5_ok(url = "https://example.com"):
    # ok: insecure-urlopener-retrieve
    URLopener().retrieve(url)
    
def test6(url = "http://example.com"):
    od = URLopener()
    # ruleid: insecure-urlopener-retrieve
    od.retrieve(url)
    
def test6_ok(url = "https://example.com"):
    od = URLopener()
    # ok: insecure-urlopener-retrieve
    od.retrieve(url)