from cryptography.hazmat.primitives import hashes

def ex2(user, pwtext):
    md5 = hashes.MD5()
    # ruleid: md5-used-as-password
    user.setPassword(md5)
