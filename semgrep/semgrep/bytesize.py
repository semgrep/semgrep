#
# Parse command-line arguments representing a number of bytes, such as
# '5 mb' or '3.2GiB'.
#
# Maybe we could use an external package for this, such as
# https://pypi.org/project/datasize/ (documentation?)
# or https://github.com/xolox/python-humanfriendly
#
import re

UNITS = {
    "": 1,
    "B": 1,
    "KIB": 2 ** 10,
    "MIB": 2 ** 20,
    "GIB": 2 ** 30,
    "TIB": 2 ** 40,
    "KB": 10 ** 3,
    "MB": 10 ** 6,
    "GB": 10 ** 9,
    "TB": 10 ** 12,
}


def parse_size(input: str) -> int:
    s = input.upper()
    # note that '1e6' is a valid float and should not become '1 e6'.
    s = re.sub(r"([BKMGT][A-Z]*)", r" \1", s)
    tokens = [sub.strip() for sub in s.split()]
    n = len(tokens)
    if n == 1:
        number = tokens[0]
        unit = ""
    elif n == 2:
        number, unit = tokens
    else:
        raise ValueError(f"Invalid representation for a number of bytes: '{input}'")
    if unit in UNITS:
        return int(float(number) * UNITS[unit])
    else:
        raise ValueError(f"Invalid representation for a number of bytes: '{input}'")


TESTS = [
    # test format
    ("42", 42),
    ("1.4", 1),
    ("7.9e6", 7900000),
    ("  99  ", 99),
    ("456.7  MB", 456700000),
    ("456.7MB", 456700000),
    # test decimal units
    ("1234b", 1234),
    ("1512 KB", 1512000),
    ("6 mb", 6000000),
    ("7 GB", 7 * 1000 * 1000 * 1000),
    ("8 TB", 8 * 1000 * 1000 * 1000 * 1000),
    # test binary units
    ("1512 KiB", 1512 * 1024),
    ("7 GiB", 7 * 1024 * 1024 * 1024),
    ("8 TiB", 8 * 1024 * 1024 * 1024 * 1024),
]


def test_parse_size() -> None:
    for input, expected_output in TESTS:
        assert parse_size(input) == expected_output
