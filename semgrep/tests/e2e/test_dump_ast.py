def test_dump_ast(run_semgrep_in_tmp, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=["--dump-ast", "--lang", "python"],
    )
    snapshot.assert_match(stdout, "results.json")


def test_dump_ast_no_lang(run_semgrep_in_tmp, snapshot):
    _, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=["--dump-ast", "python"],
        fail_on_nonzero=False,
    )
    snapshot.assert_match(stderr, "error.txt")
