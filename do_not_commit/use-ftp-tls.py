import ftplib
import ssl

def bad():
    # ruleid: use-ftp-tls
    ftpc = ftplib.FTP("example.com", "user", "pass")

def ok():
    # ok: use-ftp-tls
    ftpc = ftplib.FTP_TLS("example.com", "user", "pass", context=ssl.create_default_context())