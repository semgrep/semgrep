from urllib.request import urlopen 

def test1():
    # ruleid: insecure-urlopen
    urlopen("http://example.com")

def test1_ok():
    # ok: insecure-urlopen
    urlopen("https://example.com")

def test2():
    # ruleid: insecure-urlopen
    url = "http://example.com"
    urlopen(url)

def test2_ok():
    # ok: insecure-urlopen
    url = "https://example.com"
    urlopen(url)

# ruleid: insecure-urlopen
def test3(url = "http://example.com"):
    urlopen(url)
    
# ok: insecure-urlopen
def test3_ok(url = "https://example.com"):
    urlopen(url)
    