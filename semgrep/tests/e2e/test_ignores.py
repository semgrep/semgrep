import subprocess
from pathlib import Path

from ..conftest import _clean_output_json
from ..conftest import TESTS_PATH


def test_semgrepignore(run_semgrep_in_tmp, tmp_path, snapshot):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TESTS_PATH / "e2e" / "targets" / "ignores" / ".semgrepignore").resolve()
    )

    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq-js.yaml", target_name="ignores")[0],
        "results.json",
    )


# Input from stdin will not have a path that is relative to tmp_path, where we're running semgrep
def test_file_not_relative_to_base_path(tmp_path, monkeypatch, snapshot):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TESTS_PATH / "e2e" / "targets" / "ignores" / ".semgrepignore").resolve()
    )
    monkeypatch.chdir(tmp_path)
    process = subprocess.Popen(
        [
            "python3",
            "-m",
            "semgrep",
            "--disable-version-check",
            "--metrics",
            "off",
            "--json",
            "-e",
            "a",
            "--lang",
            "js",
            "-",
        ],
        encoding="utf-8",
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    stdout, _ = process.communicate("a")
    snapshot.assert_match(_clean_output_json(stdout), "results.json")
