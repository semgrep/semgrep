import pytest

from semgrep.bytesize import parse_size


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


@pytest.mark.quick
def test_parse_size() -> None:
    for input, expected_output in TESTS:
        assert parse_size(input) == expected_output
