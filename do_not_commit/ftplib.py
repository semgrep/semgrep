# cf. https://github.com/PyCQA/bandit/blob/d5f8fa0d89d7b11442fc6ec80ca42953974354c8/examples/ftplib.py

from ftplib import FTP

# ruleid:ftplib
ftp = FTP('ftp.debian.org')
ftp.login()

ftp.cwd('debian')
ftp.retrlines('LIST')

ftp.quit()