"""
Tests for semgrep.metrics and associated command-line arguments.
"""
import json
import os
import re
import sys
import time
import uuid
from pathlib import Path
from shutil import copytree
from typing import Iterator

import dateutil.tz
import freezegun.api
import pytest
from pytest import mark
from pytest import MonkeyPatch
from tests.conftest import RULES_PATH
from tests.conftest import TARGETS_PATH
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SemgrepRunner

from semgrep.cli import cli
from semgrep.config_resolver import ConfigFile


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
        "semgrep.config_resolver.ConfigLoader._download_config_from_url",
        lambda s, url: ConfigFile(None, USELESS_EQEQ, url),
    )
    yield


@pytest.mark.kinda_slow
@mark.parametrize(
    "config,metrics_flag,metrics_env,should_send",
    [
        ("rules/eqeq.yaml", None, None, False),
        ("r/python.lang.correctness.useless-eqeq.useless-eqeq", None, None, True),
        ("rules/eqeq.yaml", "auto", None, False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            "auto",
            None,
            True,
        ),
        ("rules/eqeq.yaml", "on", None, True),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            "on",
            None,
            True,
        ),
        ("rules/eqeq.yaml", "off", None, False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            "off",
            None,
            False,
        ),
        ("rules/eqeq.yaml", None, "auto", False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            None,
            "auto",
            True,
        ),
        ("rules/eqeq.yaml", None, "off", False),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            None,
            "off",
            False,
        ),
        ("rules/eqeq.yaml", None, "on", True),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            None,
            "on",
            True,
        ),
        (
            "rules/eqeq.yaml",
            "auto",
            "on",
            False,
        ),
        (
            "r/python.lang.correctness.useless-eqeq.useless-eqeq",
            "auto",
            "off",
            True,
        ),
    ],
)
@pytest.mark.osemfail
def test_flags(
    run_semgrep_in_tmp: RunSemgrep,
    mock_config_request,
    config,
    metrics_flag,
    metrics_env,
    should_send,
):
    """
    Test that we try to send metrics when we should be
    """
    options = ["--metrics", metrics_flag] if metrics_flag is not None else []
    env = {"SEMGREP_SEND_METRICS": metrics_env} if metrics_env is not None else {}
    _, stderr = run_semgrep_in_tmp(
        config,
        options=[*options, "--debug"],
        force_metrics_off=False,
        env=env,
    )
    if should_send:
        assert "Sending pseudonymous metrics" in stderr
        assert "Not sending pseudonymous metrics" not in stderr
    else:
        assert "Sending pseudonymous metrics" not in stderr
        assert "Not sending pseudonymous metrics" in stderr


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_flags_actual_send(run_semgrep_in_tmp: RunSemgrep):
    """
    Test that the server for metrics sends back success
    """
    _, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--debug"],
        env={"SEMGREP_SEND_METRICS": "on"},
        force_metrics_off=False,
    )
    assert "Sending pseudonymous metrics" in stderr
    assert "Failed to send pseudonymous metrics" not in stderr


# This is meant to test a specific JSON string field.
# Make this simpler, coarser?
def _mask_digits(value: str) -> str:
    return re.sub(r"\d+", "<MASKED DIGITS>", value)


# What calls this? What's the type of the argument? Why are they not the
# same arguments as the other test functions?
@pytest.mark.quick
@pytest.mark.freeze_time("2017-03-03")
@pytest.mark.skipif(
    sys.version_info < (3, 8),
    reason="snapshotting mock call kwargs doesn't work on py3.7",
)
@mark.parametrize("pro_flag", [[]])  # TODO add back test for pro, ["--pro"]])
@pytest.mark.osemfail
def test_metrics_payload(tmp_path, snapshot, mocker, monkeypatch, pro_flag):
    # make the formatted timestamp strings deterministic
    mocker.patch.object(
        freezegun.api, "tzlocal", return_value=dateutil.tz.gettz("Asia/Tokyo")
    )
    original_tz = os.environ.get("TZ")
    os.environ["TZ"] = "Asia/Tokyo"
    time.tzset()

    # this makes event_id deterministic
    mocker.patch("semgrep.metrics.get_frozen_id", return_value=uuid.UUID("0" * 32))
    mocker.patch("semgrep.metrics.mock_float", return_value=0.0)
    mocker.patch("semgrep.metrics.mock_int", return_value=0)

    mock_post = mocker.patch("requests.post")

    (tmp_path / ".settings.yaml").write_text(
        f"anonymous_user_id: {str(uuid.UUID('1' * 32))}"
    )
    copytree(
        Path(TARGETS_PATH / "metrics_send").resolve(),
        tmp_path / "metrics_files",
    )
    (tmp_path / "rule.yaml").symlink_to(RULES_PATH / "metrics_send" / "deep.yaml")
    monkeypatch.chdir(tmp_path)

    runner = SemgrepRunner(
        env={
            "SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml"),
            "SEMGREP_INTEGRATION_NAME": "funkyintegration",
        },
        use_click_runner=True,
    )
    runner.invoke(
        cli,
        subcommand="scan",
        args=["--config=rule.yaml", "--metrics=on", "metrics_files", "--time"]
        + pro_flag,
    )

    # TODO: simplify - use regexps on the raw output, such as
    # one that hides any number with more than 5 digits.
    payload = json.loads(mock_post.call_args.kwargs["data"])
    payload["environment"]["version"] = _mask_digits(payload["environment"]["version"])
    payload["environment"]["isAuthenticated"] = False
    payload["performance"]["maxMemoryBytes"] = _mask_digits(
        str(payload["performance"]["maxMemoryBytes"])
    )

    snapshot.assert_match(
        json.dumps(payload, indent=2, sort_keys=True), "metrics-payload.json"
    )

    if original_tz is not None:
        os.environ["TZ"] = original_tz
    else:
        del os.environ["TZ"]
    time.tzset()
