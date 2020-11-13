# cf. https://github.com/PyCQA/bandit/blob/694dfaa370cce54ea23169123554598bad0e1be6/examples/urlopen.py

''' Example dangerous usage of urllib[2] opener functions

The urllib and urllib2 opener functions and object can open http, ftp,
and file urls. Often, the ability to open file urls is overlooked leading
to code that can unexpectedly open files on the local server. This
could be used by an attacker to leak information about the server.
'''


import urllib
import urllib2

# Python 3
import urllib.request

def test_urlopen():
    # urllib
    url = urllib.quote('file:///bin/ls')
    # ruleid:dynamic-urllib-use-detected
    urllib.urlopen(url, 'blah', 32)

    # Detect this because it can retrieve any number of args. Hard to detect with Semgrep.
    # ruleid:dynamic-urllib-use-detected
    urllib.urlretrieve('file:///bin/ls', '/bin/ls2')
    opener = urllib.URLopener()

    # This is OK because it's a constant.
    # ok
    opener.open('file:///bin/ls')
    # ok
    opener.retrieve('file:///bin/ls')
    opener2 = urllib.FancyURLopener()
    # ok
    opener2.open('file:///bin/ls')
    # ok
    opener2.retrieve('file:///bin/ls')

    # ruleid:dynamic-urllib-use-detected
    opener.open(url)
    # ruleid:dynamic-urllib-use-detected
    opener.retrieve(url)
    # ruleid:dynamic-urllib-use-detected
    opener2.open(url)
    # ruleid:dynamic-urllib-use-detected
    opener2.retrieve(url)

    # Python 3
    # ok
    urllib.request.urlopen('file:///bin/ls')
    # ruleid:dynamic-urllib-use-detected
    urllib.request.urlretrieve('file:///bin/ls', '/bin/ls2')
    opener = urllib.request.URLopener()
    # ok
    opener.open('file:///bin/ls')
    # ok
    opener.retrieve('file:///bin/ls')
    opener2 = urllib.request.FancyURLopener()
    # ok
    opener2.open('file:///bin/ls')
    # ok
    opener2.retrieve('file:///bin/ls')
