from pathlib import Path
from typing import FrozenSet
from typing import List
from typing import Set
from unittest import mock

import pytest

from semgrep.util import log_removed_paths


def _to_path_frozenset(path_strings: List[str]) -> FrozenSet[Path]:
    return frozenset(Path(p) for p in path_strings)


PATHS = _to_path_frozenset(["foo", "bar", "baz"])


@pytest.mark.parametrize(
    "return_value, expected_log",
    [
        ([], ["foo", "bar", "baz"]),
        (["foo"], ["bar", "baz"]),
        (["foo", "bar", "baz"], []),
    ],
)
def test_log_removed_paths(return_value, expected_log):
    mock_filter = mock.Mock(return_value=_to_path_frozenset(return_value))
    decorated_filter = log_removed_paths(mock_filter)
    removal_log: Set[Path] = set()
    assert mock_filter(candidates=PATHS) == decorated_filter(
        candidates=PATHS, removal_log=removal_log
    ), "should not change result of filtering"
    assert removal_log == _to_path_frozenset(expected_log), "should log removed paths"


def test_log_removed_paths__missing_removal_log():
    mock_filter = mock.Mock(return_value=frozenset([Path("foo")]))
    decorated_filter = log_removed_paths(mock_filter)
    assert mock_filter(candidates=PATHS) == decorated_filter(
        candidates=PATHS
    ), "should not change result of filtering"
    assert decorated_filter(candidates=PATHS), "should not error when not logging"
