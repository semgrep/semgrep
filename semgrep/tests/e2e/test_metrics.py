"""
Tests for semgrep.metrics and associated command-line arguments.
"""
import json
import os
import re
import sys
import time
import uuid
from typing import Iterator

import dateutil.tz
import freezegun.api
import pytest
from click.testing import CliRunner
from pytest import mark
from pytest import MonkeyPatch

from semgrep.cli import cli
from semgrep.profiling import ProfilingData
from tests.conftest import TESTS_PATH


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


@pytest.mark.kinda_slow
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
    if should_send:
        assert "Sending pseudonymous metrics" in output
        assert "Not sending pseudonymous metrics" not in output
    else:
        assert "Sending pseudonymous metrics" not in output
        assert "Not sending pseudonymous metrics" in output


@pytest.mark.kinda_slow
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
    assert "Sending pseudonymous metrics" in output
    assert "Failed to send pseudonymous metrics" not in output


@pytest.mark.slow
def test_legacy_flags(run_semgrep_in_tmp):
    """
    Test metrics sending respects legacy flags. Flags take precedence over envvar
    """
    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug", "--enable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sending pseudonymous metrics" in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug", "--enable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": ""},
    )
    assert "Sending pseudonymous metrics" in output

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug", "--disable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing"},
    )
    assert "Sending pseudonymous metrics" not in output

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

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--disable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "off"},
        fail_on_nonzero=False,
    )
    assert (
        "--enable-metrics/--disable-metrics can not be used with either --metrics or SEMGREP_SEND_METRICS"
        not in output
    )

    _, output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--enable-metrics"],
        env={"SEMGREP_USER_AGENT_APPEND": "testing", "SEMGREP_SEND_METRICS": "on"},
        fail_on_nonzero=False,
    )
    assert (
        "--enable-metrics/--disable-metrics can not be used with either --metrics or SEMGREP_SEND_METRICS"
        not in output
    )


def _mask_version(value: str) -> str:
    return re.sub(r"\d+", "x", value)


@pytest.mark.quick
@pytest.mark.freeze_time("2017-03-03")
@pytest.mark.skipif(
    sys.version_info < (3, 8),
    reason="snapshotting mock call kwargs doesn't work on py3.7",
)
def test_metrics_payload(tmp_path, snapshot, mocker, monkeypatch):
    # make the formatted timestamp strings deterministic
    mocker.patch.object(
        freezegun.api, "tzlocal", return_value=dateutil.tz.gettz("Asia/Tokyo")
    )
    original_tz = os.environ.get("TZ")
    os.environ["TZ"] = "Asia/Tokyo"
    time.tzset()

    # make the rule and file timings deterministic
    mocker.patch.object(ProfilingData, "set_file_times")
    mocker.patch.object(ProfilingData, "set_rules_parse_time")

    # make the event ID deterministic
    mocker.patch("uuid.uuid4", return_value=uuid.UUID("0" * 32))

    mock_post = mocker.patch("requests.post")

    (tmp_path / ".settings.yaml").write_text(
        f"anonymous_user_id: {str(uuid.UUID('1' * 32))}"
    )
    (tmp_path / "code.py").write_text("5 == 5")
    (tmp_path / "rule.yaml").symlink_to(TESTS_PATH / "e2e" / "rules" / "eqeq.yaml")
    monkeypatch.chdir(tmp_path)

    runner = CliRunner(env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")})
    runner.invoke(cli, ["scan", "--config=rule.yaml", "--metrics=on", "code.py"])

    payload = json.loads(mock_post.call_args.kwargs["data"])
    payload["environment"]["version"] = _mask_version(payload["environment"]["version"])
    payload["environment"]["isAuthenticated"] = False

    snapshot.assert_match(
        json.dumps(payload, indent=2, sort_keys=True), "metrics-payload.json"
    )

    if original_tz is not None:
        os.environ["TZ"] = original_tz
    else:
        del os.environ["TZ"]
    time.tzset()
