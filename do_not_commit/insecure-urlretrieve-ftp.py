from urllib.request import urlretrieve 

def test1():
    # ruleid: insecure-urlretrieve-ftp
    urlretrieve("ftp://example.com")

def test1_ok():
    # ok: insecure-urlretrieve-ftp
    urlretrieve("sftp://example.com")

def test2():
    # ruleid: insecure-urlretrieve-ftp
    url = "ftp://example.com"
    urlretrieve(url)

def test2_ok():
    # ok: insecure-urlretrieve-ftp
    url = "sftp://example.com"
    urlretrieve(url)

# ruleid: insecure-urlretrieve-ftp
def test3(url = "ftp://example.com"):
    urlretrieve(url)
    
# ok: insecure-urlretrieve-ftp
def test3_ok(url = "sftp://example.com"):
    urlretrieve(url)
    