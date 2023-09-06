from pathlib import Path

import pytest
from tests.conftest import _clean_output_json
from tests.fixtures import RunSemgrep

from ..conftest import TESTS_PATH


@pytest.mark.osempass
@pytest.mark.kinda_slow
def test_semgrepignore(run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TESTS_PATH / "e2e" / "targets" / "ignores" / ".semgrepignore").resolve()
    )

    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq-basic.yaml", target_name="ignores").stdout,
        "results.json",
    )


# We provide no .semgrepignore but everything except find.js should still be ignored
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
def test_file_not_relative_to_base_path(run_semgrep: RunSemgrep, snapshot):
    results = run_semgrep(
        options=["--json", "-e", "a", "--lang", "js", "-"],
        stdin="a",
        use_click_runner=True,  # TODO: probably because of stdin?
    )
    results.raw_stdout = _clean_output_json(results.raw_stdout, True)
    snapshot.assert_match(results.as_snapshot(), "results.txt")


@pytest.mark.kinda_slow
def test_internal_explicit_semgrepignore(
    run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot
):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TESTS_PATH / "e2e" / "targets" / "ignores" / ".semgrepignore").resolve()
    )

    explicit_ignore_file = tmp_path / ".semgrepignore_explicit"
    explicit_ignore_file.touch()

    env = {"SEMGREP_R2C_INTERNAL_EXPLICIT_SEMGREPIGNORE": str(explicit_ignore_file)}
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-basic.yaml", target_name="ignores", env=env
        ).stdout,
        "results.json",
    )
