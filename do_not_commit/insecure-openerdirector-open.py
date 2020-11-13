from urllib.request import OpenerDirector

def test1():
    od = OpenerDirector()
    # ruleid: insecure-openerdirector-open
    od.open("http://example.com")

def test1_ok():
    od = OpenerDirector()
    # ok: insecure-openerdirector-open
    od.open("https://example.com")

def test2():
    od = OpenerDirector()
    # ruleid: insecure-openerdirector-open
    url = "http://example.com"
    od.open(url)

def test2_ok():
    od = OpenerDirector()
    # ok: insecure-openerdirector-open
    url = "https://example.com"
    od.open(url)

def test3():
    # ruleid: insecure-openerdirector-open
    OpenerDirector().open("http://example.com")

def test3_ok():
    # ok: insecure-openerdirector-open
    OpenerDirector().open("https://example.com")

def test4():
    # ruleid: insecure-openerdirector-open
    url = "http://example.com"
    OpenerDirector().open(url)

def test4_ok():
    # ok: insecure-openerdirector-open
    url = "https://example.com"
    OpenerDirector().open(url)

def test5(url = "http://example.com"):
    # ruleid: insecure-openerdirector-open
    OpenerDirector().open(url)
    
def test5_ok(url = "https://example.com"):
    # ok: insecure-openerdirector-open
    OpenerDirector().open(url)
    
def test6(url = "http://example.com"):
    od = OpenerDirector()
    # ruleid: insecure-openerdirector-open
    od.open(url)
    
def test6_ok(url = "https://example.com"):
    od = OpenerDirector()
    # ok: insecure-openerdirector-open
    od.open(url)