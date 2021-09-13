from ruamel.yaml import YAML

from semgrep.error import SourceParseError

yaml = YAML()


def test_different_hash():
    # SemgrepErrors with differing fields have different hash
    error_1 = SourceParseError(
        short_msg="1",
        long_msg="2",
        spans=[],
        help="4",
    )

    error_2 = SourceParseError(
        short_msg="not 1",
        long_msg="2",
        spans=[],
        help="4",
    )

    assert error_1.__hash__() != error_2.__hash__()

    errors = set()
    errors.add(error_1)
    assert error_2 not in errors


def test_same_hash():
    # SemgrepErrors with all same fields have the same hash
    error_1 = SourceParseError(
        short_msg="1",
        long_msg="2",
        spans=[],
        help="4",
    )

    error_2 = SourceParseError(
        short_msg="1",
        long_msg="2",
        spans=[],
        help="4",
    )

    assert error_1.__hash__() == error_2.__hash__()

    errors = set()
    errors.add(error_1)
    assert error_2 in errors
