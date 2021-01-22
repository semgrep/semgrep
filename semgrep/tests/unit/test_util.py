import pytest

from semgrep import util


@pytest.mark.parametrize(
    "test_input,expected",
    [
        ({}, False),
        ({"key": "value"}, True),
        ({"key1": "value"}, False),
        ({"key1": {"key": "value"}}, True),
        ({"key1": {"key2": "value"}}, False),
        ({"key1": [{"key": "value"}]}, True),
        ({"key1": [{"key2": "value"}]}, False),
        ({"key1": [{"key2": "value"}, {"key": "value"}]}, True),
        ({"key1": [{"key2": "value"}, {"key3": "value"}]}, False),
        ({"key1": [{"key2": "value"}, {"key3": {"key": "value"}}]}, True),
        ({"key1": [{"key2": "value"}, {"key3": {"key4": "value"}}]}, False),
        ({"zzz": {"aaa": "value", "key": "value"}}, True),
        ({"zzz": {"aaa": "value", "key1": "value"}}, False),
        ({"bbb": {"aaa": "value"}, "zzz": {"key": "value"}}, True),
        ({"bbb": {"aaa": "value"}, "zzz": {"key1": "value"}}, False),
    ],
)
def test_recursive_has_key(test_input, expected):
    assert util.recursive_has_key(lambda d: "key" in d, test_input) == expected
