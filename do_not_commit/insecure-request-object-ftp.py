from urllib.request import Request

def test1():
    # ruleid: insecure-request-object-ftp
    Request("ftp://example.com")

def test1_ok():
    # ok: insecure-request-object-ftp
    Request("sftp://example.com")

def test2():
    # ruleid: insecure-request-object-ftp
    url = "ftp://example.com"
    Request(url)

def test2_ok():
    # ok: insecure-request-object-ftp
    url = "sftp://example.com"
    Request(url)

# ruleid: insecure-request-object-ftp
def test3(url = "ftp://example.com"):
    Request(url)
    
# ok: insecure-request-object-ftp
def test3_ok(url = "sftp://example.com"):
    Request(url)
    