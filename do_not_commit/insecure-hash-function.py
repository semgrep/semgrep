# cf. https://github.com/PyCQA/bandit/blob/b1411bfb43795d3ffd268bef17a839dee954c2b1/examples/hashlib_new_insecure_functions.py

import hashlib

# ruleid:insecure-hash-function
hashlib.new('md5')

# ruleid:insecure-hash-function
hashlib.new('md4', 'test')

# ruleid:insecure-hash-function
hashlib.new(name='md5', string='test')

# ruleid:insecure-hash-function
hashlib.new('MD4', string='test')

# ruleid:insecure-hash-function
hashlib.new(string='test', name='MD5')

# ok
hashlib.new('sha256')

# ok
hashlib.new('SHA512')