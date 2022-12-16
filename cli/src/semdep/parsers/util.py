from parsy import regex
from typing import Dict
from typing import List
from base64 import b64decode
from base64 import b16encode

def not_any(chars):
    return regex(f"[^{''.join(chars)}]*")

def extract_npm_lockfile_hash(s: str) -> Dict[str, List[str]]:
    """
    Go from:
        sha512-aePbxDmcYW++PaqBsJ+HYUFwCdv4LVvdnhBy78E57PIor8/OVvhMrADFFEDh8DHDFRv/O9i3lPhsENjO7QX0+A==
    To:
        sha512,
    """
    algorithm = s.split("-")[0]
    rest = s[len(algorithm) + 1 :]
    decode_base_64 = b64decode(rest)
    return {algorithm: [b16encode(decode_base_64).decode("ascii").lower()]}
