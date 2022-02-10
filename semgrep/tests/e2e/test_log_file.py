def test_last_log_exists(run_semgrep_in_tmp, tmp_path, snapshot):
    log_dest = tmp_path / "foo" / "bar" / "last.log"
    run_semgrep_in_tmp("rules/eqeq.yaml", env={"SEMGREP_LOG_FILE": str(log_dest)})

    log = log_dest.read_text()
    assert "- DEBUG -" in log
    assert "- INFO -" in log
    assert "- VERBOSE -" in log
