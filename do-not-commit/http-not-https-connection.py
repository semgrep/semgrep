import urllib3 as ur3

# ruleid:http-not-https-connection
pool = ur3.connectionpool.HTTPConnectionPool("example.com")

# ok
spool = ur3.connectionpool.HTTPSConnectionPool("example.com")
