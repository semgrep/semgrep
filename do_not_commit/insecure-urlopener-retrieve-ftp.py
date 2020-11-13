from urllib.request import URLopener

def test1():
    od = URLopener()
    # ruleid: insecure-urlopener-retrieve-ftp
    od.retrieve("ftp://example.com")

def test1_ok():
    od = URLopener()
    # ok: insecure-urlopener-retrieve-ftp
    od.retrieve("ftps://example.com")

def test2():
    od = URLopener()
    # ruleid: insecure-urlopener-retrieve-ftp
    url = "ftp://example.com"
    od.retrieve(url)

def test2_ok():
    od = URLopener()
    # ok: insecure-urlopener-retrieve-ftp
    url = "ftps://example.com"
    od.retrieve(url)

def test3():
    # ruleid: insecure-urlopener-retrieve-ftp
    URLopener().retrieve("ftp://example.com")

def test3_ok():
    # ok: insecure-urlopener-retrieve-ftp
    URLopener().retrieve("ftps://example.com")

def test4():
    # ruleid: insecure-urlopener-retrieve-ftp
    url = "ftp://example.com"
    URLopener().retrieve(url)

def test4_ok():
    # ok: insecure-urlopener-retrieve-ftp
    url = "ftps://example.com"
    URLopener().retrieve(url)

def test5(url = "ftp://example.com"):
    # ruleid: insecure-urlopener-retrieve-ftp
    URLopener().retrieve(url)
    
def test5_ok(url = "ftps://example.com"):
    # ok: insecure-urlopener-retrieve-ftp
    URLopener().retrieve(url)
    
def test6(url = "ftp://example.com"):
    od = URLopener()
    # ruleid: insecure-urlopener-retrieve-ftp
    od.retrieve(url)
    
def test6_ok(url = "ftps://example.com"):
    od = URLopener()
    # ok: insecure-urlopener-retrieve-ftp
    od.retrieve(url)