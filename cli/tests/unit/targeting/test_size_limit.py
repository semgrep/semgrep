from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest

from semgrep.target_manager import TargetManager


@pytest.mark.quick
@pytest.mark.parametrize(
    "size_limit,should_skip",
    [
        pytest.param(-1, False, id="no max size (-1)"),
        pytest.param(0, False, id="no max size (0)"),
        pytest.param(5, True, id="file is over max size"),
        pytest.param(10, False, id="file is exactly max size"),
        pytest.param(20, False, id="file is under max size"),
    ],
)
def test_filter_by_size(size_limit, should_skip):
    with NamedTemporaryFile() as fp:
        fp.write(b"0123456789")
        fp.flush()
        path = Path(fp.name)
        targets = frozenset({path})

        skipped = bool(
            TargetManager.filter_by_size(size_limit, candidates=targets).removed
        )

    assert skipped == should_skip
