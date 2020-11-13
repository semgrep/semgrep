from urllib.request import OpenerDirector

def test1():
    od = OpenerDirector()
    # ruleid: insecure-openerdirector-open-ftp
    od.open("ftp://example.com")

def test1_ok():
    od = OpenerDirector()
    # ok: insecure-openerdirector-open-ftp
    od.open("sftp://example.com")

def test2():
    od = OpenerDirector()
    # ruleid: insecure-openerdirector-open-ftp
    url = "ftp://example.com"
    od.open(url)

def test2_ok():
    od = OpenerDirector()
    # ok: insecure-openerdirector-open-ftp
    url = "sftp://example.com"
    od.open(url)

def test3():
    # ruleid: insecure-openerdirector-open-ftp
    OpenerDirector().open("ftp://example.com")

def test3_ok():
    # ok: insecure-openerdirector-open-ftp
    OpenerDirector().open("sftp://example.com")

def test4():
    # ruleid: insecure-openerdirector-open-ftp
    url = "ftp://example.com"
    OpenerDirector().open(url)

def test4_ok():
    # ok: insecure-openerdirector-open-ftp
    url = "sftp://example.com"
    OpenerDirector().open(url)

def test5(url = "ftp://example.com"):
    # ruleid: insecure-openerdirector-open-ftp
    OpenerDirector().open(url)
    
def test5_ok(url = "sftp://example.com"):
    # ok: insecure-openerdirector-open-ftp
    OpenerDirector().open(url)
    
def test6(url = "ftp://example.com"):
    od = OpenerDirector()
    # ruleid: insecure-openerdirector-open-ftp
    od.open(url)
    
def test6_ok(url = "sftp://example.com"):
    od = OpenerDirector()
    # ok: insecure-openerdirector-open-ftp
    od.open(url)