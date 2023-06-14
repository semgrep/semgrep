import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_dump_ast(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=["--dump-ast", "--lang", "python"],
    )
    snapshot.assert_match(stdout, "results.json")


@pytest.mark.kinda_slow
def test_dump_ast_no_lang(run_semgrep_in_tmp: RunSemgrep, snapshot):
    _, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=["--dump-ast", "python"],
        assert_exit_code=2,
    )
    snapshot.assert_match(stderr, "error.txt")
