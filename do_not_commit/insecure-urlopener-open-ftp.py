from urllib.request import URLopener

def test1():
    od = URLopener()
    # ruleid: insecure-urlopener-open-ftp
    od.open("ftp://example.com")

def test1_ok():
    od = URLopener()
    # ok: insecure-urlopener-open-ftp
    od.open("ftps://example.com")

def test2():
    od = URLopener()
    # ruleid: insecure-urlopener-open-ftp
    url = "ftp://example.com"
    od.open(url)

def test2_ok():
    od = URLopener()
    # ok: insecure-urlopener-open-ftp
    url = "ftps://example.com"
    od.open(url)

def test3():
    # ruleid: insecure-urlopener-open-ftp
    URLopener().open("ftp://example.com")

def test3_ok():
    # ok: insecure-urlopener-open-ftp
    URLopener().open("ftps://example.com")

def test4():
    # ruleid: insecure-urlopener-open-ftp
    url = "ftp://example.com"
    URLopener().open(url)

def test4_ok():
    # ok: insecure-urlopener-open-ftp
    url = "ftps://example.com"
    URLopener().open(url)

def test5(url = "ftp://example.com"):
    # ruleid: insecure-urlopener-open-ftp
    URLopener().open(url)
    
def test5_ok(url = "ftps://example.com"):
    # ok: insecure-urlopener-open-ftp
    URLopener().open(url)
    
def test6(url = "ftp://example.com"):
    od = URLopener()
    # ruleid: insecure-urlopener-open-ftp
    od.open(url)
    
def test6_ok(url = "ftps://example.com"):
    od = URLopener()
    # ok: insecure-urlopener-open-ftp
    od.open(url)