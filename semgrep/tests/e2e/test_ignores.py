from pathlib import Path

from ..conftest import TESTS_PATH


def test_semgrepignore(run_semgrep_in_tmp, tmp_path, snapshot):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TESTS_PATH / "e2e" / "targets" / "ignores" / ".semgrepignore").resolve()
    )

    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq-js.yaml", target_name="ignores")[0],
        "results.json",
    )
