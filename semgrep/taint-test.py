import hashlib
from cryptography.hazmat.primitives import hashes
from Crypto.Hash import MD5, SHA256

#### True Positives ####
def ex1(user, pwtext):
    md5 = hashlib.md5(pwtext).hexdigest()
    # ruleid: md5-used-as-password
    user.setPassword(md5)

def ex2(user, pwtext):
    digest = hashes.Hash(hashes.MD5())
    digest.update(bytes(pwtext))
    # ruleid: md5-used-as-password
    user.setPassword(digest.finalize())

def ex3(user, pwtext):
    h = MD5.new()
    h.update(bytes(pwtext))
    # ruleid: md5-used-as-password
    user.setPassword(h.hexdigest(), None, None)

#### True Negatives ####
def ok1(user, pwtext):
    sha = hashlib.sha256(pwtext).hexdigest()
    # ok: md5-used-as-password
    user.setPassword(sha)

def ok2(user, pwtext):
    digest = hashes.Hash(hashes.SHA256())
    digest.update(bytes(pwtext))
    # ok: md5-used-as-password
    user.setPassword(digest.finalize())

def ok3(user, pwtext):
    h = SHA256.new()
    h.update(bytes(pwtext))
    # ok: md5-used-as-password
    user.setPassword(h.hexdigest())

def ok4(user, pwtext):
    h = MD5.new()
    h.update(bytes(pwtext))
    # ok: md5-used-as-password
    user.updateSomethingElse(h.hexdigest())