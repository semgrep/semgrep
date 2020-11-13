from urllib.request import urlretrieve

def test1():
    # ruleid: insecure-urlretrieve
    urlretrieve("http://example.com")

def test1_ok():
    # ok: insecure-urlretrieve
    urlretrieve("https://example.com")

def test2():
    # ruleid: insecure-urlretrieve
    url = "http://example.com"
    urlretrieve(url)

def test2_ok():
    # ok: insecure-urlretrieve
    url = "https://example.com"
    urlretrieve(url)

# ruleid: insecure-urlretrieve
def test3(url = "http://example.com"):
    urlretrieve(url)
    
# ok: insecure-urlretrieve
def test3_ok(url = "https://example.com"):
    urlretrieve(url)
    