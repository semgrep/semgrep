def test_envvar(run_semgrep_in_tmp):
    """
    Test metrics sending respects SEMGREP_SEND_METRICS envvar
    """
    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "1"},
    )
    assert "Sent non-identifiable metrics" in output

    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "true"},
    )
    assert "Sent non-identifiable metrics" in output

    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": ""},
    )
    assert "Sent non-identifiable metrics" not in output

    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sent non-identifiable metrics" not in output


def test_flags(run_semgrep_in_tmp):
    """
    Test metrics sending respects flags. Flags take precedence over envvar
    """
    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug", "--enable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sent non-identifiable metrics" in output

    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug", "--enable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": ""},
    )
    assert "Sent non-identifiable metrics" in output

    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug", "--disable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sent non-identifiable metrics" not in output

    output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        stderr=True,
        options=["--debug", "--disable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "1"},
    )
    assert "Sent non-identifiable metrics" not in output
