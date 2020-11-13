from urllib.request import URLopener

def test1():
    od = URLopener()
    # ruleid: insecure-urlopener-open
    od.open("http://example.com")

def test1_ok():
    od = URLopener()
    # ok: insecure-urlopener-open
    od.open("https://example.com")

def test2():
    od = URLopener()
    # ruleid: insecure-urlopener-open
    url = "http://example.com"
    od.open(url)

def test2_ok():
    od = URLopener()
    # ok: insecure-urlopener-open
    url = "https://example.com"
    od.open(url)

def test3():
    # ruleid: insecure-urlopener-open
    URLopener().open("http://example.com")

def test3_ok():
    # ok: insecure-urlopener-open
    URLopener().open("https://example.com")

def test4():
    # ruleid: insecure-urlopener-open
    url = "http://example.com"
    URLopener().open(url)

def test4_ok():
    # ok: insecure-urlopener-open
    url = "https://example.com"
    URLopener().open(url)

def test5(url = "http://example.com"):
    # ruleid: insecure-urlopener-open
    URLopener().open(url)
    
def test5_ok(url = "https://example.com"):
    # ok: insecure-urlopener-open
    URLopener().open(url)
    
def test6(url = "http://example.com"):
    od = URLopener()
    # ruleid: insecure-urlopener-open
    od.open(url)
    
def test6_ok(url = "https://example.com"):
    od = URLopener()
    # ok: insecure-urlopener-open
    od.open(url)