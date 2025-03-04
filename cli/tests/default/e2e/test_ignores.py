import shutil
from pathlib import Path

import pytest
from tests.conftest import mask_variable_text
from tests.conftest import TARGETS_PATH
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_semgrepignore(run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TARGETS_PATH / "ignores" / ".semgrepignore").resolve()
    )
    # BUG: pysemgrep doesn't complain if an included file is missing
    # This file is included by the .semgrepignore above.
    # It must be a regular file, not a symlink (enforced in osemgrep).
    shutil.copyfile(
        Path(TARGETS_PATH / "ignores" / ".gitignore"), tmp_path / ".gitignore"
    )

    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq-basic.yaml", target_name="ignores").stdout,
        "results.json",
    )


# We provide no .semgrepignore but everything except find.js should still
# be ignored
@pytest.mark.kinda_slow
def test_default_semgrepignore(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-basic.yaml", target_name="ignores_default"
        ).stdout,
        "results.json",
    )


# Input from stdin will not have a path that is relative to tmp_path, where we're running semgrep
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_file_not_relative_to_base_path(run_semgrep: RunSemgrep, snapshot):
    results = run_semgrep(
        options=["--json", "-e", "a", "--lang", "js", "-"],
        stdin="a",
        use_click_runner=True,  # TODO: probably because of stdin?
    )
    results.raw_stdout = mask_variable_text(results.raw_stdout)
    snapshot.assert_match(results.as_snapshot(), "results.txt")
