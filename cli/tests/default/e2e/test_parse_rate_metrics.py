"""
Tests for parse rates from semgrep.metrics.

Ensures that the parse errors reported from core are correctly picked up by the
CLI.
"""
import json
import sys
from pathlib import Path
from shutil import copytree

import pytest
from tests.conftest import TARGETS_PATH
from tests.semgrep_runner import SemgrepRunner

from semgrep.cli import cli


@pytest.mark.quick
@pytest.mark.skipif(
    sys.version_info < (3, 8),
    reason="snapshotting mock call kwargs doesn't work on py3.7",
)
@pytest.mark.osemfail
def test_parse_metrics(tmp_path, snapshot, mocker, monkeypatch):
    mock_post = mocker.patch("requests.post")

    # NOTE: for the syntax errors in the files below, changes in parsers may
    # change the exact error spans reported. The most likely scenarios for this
    # are:
    #   1. Changing a language from pfff -> tree-sitter. This may cause total
    #      parsing errors to become partial ones, as pfff doesn't report
    #      partial errors.
    #   2. Changing a tree-sitter grammar, either directly or from upstream.
    #      This might change the exact spans reported for partial parse errors.
    #   3. For language which go to a specific AST before the generic one, a
    #      change which causes an AST translation error to become a partial
    #      parsing error. Again, this would potentially cause total errors to
    #      become partial.
    # If you've made a change like the ones listed above, then updating the
    # snapshot (if it looks reasonable) should be fine.

    copytree(
        Path(TARGETS_PATH / "parse_metrics").resolve(),
        tmp_path / "parse_metrics",
    )

    monkeypatch.chdir(tmp_path / "parse_metrics")
    SemgrepRunner(use_click_runner=True).invoke(
        cli, subcommand="scan", args=["--config=rules.yaml", "--metrics=on"]
    )

    payload = json.loads(mock_post.call_args.kwargs["data"])

    snapshot.assert_match(
        json.dumps(payload["parse_rate"], indent=2, sort_keys=True), "parse-rates.json"
    )
