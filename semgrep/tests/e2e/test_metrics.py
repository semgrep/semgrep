"""
Tests for semgrep.metric_manager and associated command-line arguments.
"""
from typing import Iterator

import pytest
from pytest import mark
from pytest import MonkeyPatch


# Test data to avoid making web calls in test code
USELESS_EQEQ = """rules:
- id: python.lang.correctness.useless-eqeq.useless-eqeq
  patterns:
  - pattern-not-inside: |
      def __eq__(...):
          ...
  - pattern-not-inside: |
      def __cmp__(...):
          ...
  - pattern-not-inside: assert(...)
  - pattern-not-inside: assert ..., ...
  - pattern-not-inside: assertTrue(...)
  - pattern-not-inside: assertFalse(...)
  - pattern-either:
    - pattern: $X == $X
    - pattern: $X != $X
  - pattern-not: 1 == 1
  message: 'This expression is always True: `$X == $X` or `$X != $X`. If testing for
    floating point NaN, use `math.isnan($X)`, or `cmath.isnan($X)` if the number is
    complex.'
  languages:
  - python
  severity: ERROR
  metadata:
    category: correctness
    license: Commons Clause License Condition v1.0[LGPL-2.1-only]
    source: https://semgrep.dev/r/python.lang.correctness.useless-eqeq.useless-eqeq
"""


@pytest.fixture(scope="function")
def mock_config_request(monkeypatch: MonkeyPatch) -> Iterator[None]:
    monkeypatch.setattr(
        "semgrep.config_resolver.ConfigPath._make_config_request",
        lambda s: USELESS_EQEQ,
    )
    yield


@mark.parametrize(
    "config,options,env,should_send",
    [
        ("rules/eqeq.yaml", [], {}, False),
        ("r/python.lang.correctness.useless-eqeq.useless-eqeq", [], {}, True),
        ("rules/eqeq.yaml", ["--metrics", "auto"], {}, False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            ["--metrics", "auto"],
            {},
            True,
        ),
        ("rules/eqeq.yaml", ["--metrics", "on"], {}, True),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            ["--metrics", "on"],
            {},
            True,
        ),
        ("rules/eqeq.yaml", ["--metrics", "off"], {}, False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            ["--metrics", "off"],
            {},
            False,
        ),
        ("rules/eqeq.yaml", [], {"SEMGREP_SEND_METRICS": "auto"}, False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            [],
            {"SEMGREP_SEND_METRICS": "auto"},
            True,
        ),
        ("rules/eqeq.yaml", [], {"SEMGREP_SEND_METRICS": "off"}, False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            [],
            {"SEMGREP_SEND_METRICS": "off"},
            False,
        ),
        ("rules/eqeq.yaml", [], {"SEMGREP_SEND_METRICS": "on"}, True),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            [],
            {"SEMGREP_SEND_METRICS": "on"},
            True,
        ),
        (
            "rules/eqeq.yaml",
            ["--metrics", "auto"],
            {"SEMGREP_SEND_METRICS": "on"},
            False,
        ),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            ["--metrics", "auto"],
            {"SEMGREP_SEND_METRICS": "off"},
            True,
        ),
    ],
)
def test_flags(
    run_semgrep_in_tmp, mock_config_request, config, options, env, should_send
):
    """
    Test that we try to send metrics when we should be
    """
    _, output = run_semgrep_in_tmp(
        config,
        options=[*options, "--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", **env},
    )
    print(output)
    # Test that we try to send metrics. Even if it fails sending
    assert (
        "Sent pseudonymous metrics" in output
        or "Failed to send pseudonymous metrics" in output
        if should_send
        else "Sent pseudonymous metrics" not in output
    )


@mark.parametrize(
    "config,options,env",
    [
        ("rules/eqeq.yaml", [], {"SEMGREP_SEND_METRICS": "on"}),
    ],
)
def test_flags_actual_send(
    run_semgrep_in_tmp, mock_config_request, config, options, env
):
    """
    Test that the server for metrics sends back success
    """
    _, output = run_semgrep_in_tmp(
        config,
        options=[*options, "--debug"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", **env},
    )
    print(output)
    assert "Sent pseudonymous metrics" in output
    assert "Failed to send pseudonymous metrics" not in output


def test_legacy_flags(run_semgrep_in_tmp):
    """
    Test metrics sending respects legacy flags. Flags take precedence over envvar
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
