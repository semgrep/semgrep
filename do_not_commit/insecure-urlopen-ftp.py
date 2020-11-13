from urllib.request import urlopen 

def test1():
    # ruleid: insecure-urlopen-ftp
    urlopen("ftp://example.com")

def test1_ok():
    # ok: insecure-urlopen-ftp
    urlopen("sftp://example.com")

def test2():
    # ruleid: insecure-urlopen-ftp
    url = "ftp://example.com"
    urlopen(url)

def test2_ok():
    # ok: insecure-urlopen-ftp
    url = "sftp://example.com"
    urlopen(url)

# ruleid: insecure-urlopen-ftp
def test3(url = "ftp://example.com"):
    urlopen(url)
    
# ok: insecure-urlopen-ftp
def test3_ok(url = "sftp://example.com"):
    urlopen(url)
    