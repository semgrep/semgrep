#
# Copyright (c) 2020-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import json

import pytest
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep


# Regression test for https://github.com/semgrep/semgrep/issues/11560
# When a single file is passed as a scanning root, rule paths.include
# filtering should still work. Before the fix, the ppath was set to just
# the filename (e.g. "/included.js") instead of the full project-relative
# path, so the "included/" pattern in the directory-included rule's
# paths.include never matched.
@pytest.mark.kinda_slow
def test_paths_single_file_target(run_semgrep_in_tmp: RunSemgrep):
    result = run_semgrep_in_tmp(
        "rules/paths.yaml",
        target_name="exclude_include/included/included.js",
    )
    results = json.loads(result.stdout)["results"]
    matched_rule_ids = {r["check_id"] for r in results}
    assert any(rid.endswith("directory-included") for rid in matched_rule_ids), (
        f"paths.include filtering broken for single-file target; "
        f"matched rules: {matched_rule_ids}"
    )


# Test in-rule path filtering: paths.include, paths.exclude.
# The patterns now obey the Semgrepignore/Gitignore spec.
#
@pytest.mark.kinda_slow
def test_paths(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp("rules/paths.yaml", target_name="exclude_include").stdout,
        "results.json",
    )


# Skip on Windows: this test makes pytest hang indefinitely on Windows
# since PR https://github.com/semgrep/semgrep-proprietary/pull/3859
# Behaves fine when run as a standalone semgrep or osemgrep command.
#
# This happens because we log a warning about ambiguous rule exclude paths, and
# for some reason that message is too long and causes pytest to hang on windows
# only. Even if we split up the logger warning AND flush, it still hangs. So who
# knows, we just won't run this on Windows.
@skip_on_windows
@pytest.mark.kinda_slow
def test_paths_warnings(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/paths_warnings.yaml", target_name="exclude_include"
        ).stdout,
        "results.json",
    )
