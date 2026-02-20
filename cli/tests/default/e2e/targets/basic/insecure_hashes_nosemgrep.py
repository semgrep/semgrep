import hashlib


def calculate_checksums(file_path: str) -> dict:
    # nosemgrep
    md5 = hashlib.md5()
    # nosemgrep
    sha1 = hashlib.sha1()
