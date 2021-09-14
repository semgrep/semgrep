def test_envvar(run_semgrep_in_tmp):
    """
    Test metrics sending respects SEMGREP_SEND_METRICS envvar
    """
    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "1"},
    )
    assert "Sent pseudonymous metrics" in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "true"},
    )
    assert "Sent pseudonymous metrics" in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": ""},
    )
    assert "Sent pseudonymous metrics" not in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sent pseudonymous metrics" not in output


def test_flags(run_semgrep_in_tmp):
    """
    Test metrics sending respects flags. Flags take precedence over envvar
    """
    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug", "--enable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sent pseudonymous metrics" in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug", "--enable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": ""},
    )
    assert "Sent pseudonymous metrics" in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug", "--disable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sent pseudonymous metrics" not in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--disable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "1"},
        fail_on_nonzero=False,
    )
    assert (
        "--enable-metrics/--disable-metrics can not be used with either --metrics or SEMGREP_SEND_METRICS"
        in output
    )
