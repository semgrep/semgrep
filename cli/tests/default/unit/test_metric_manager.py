import uuid
from tempfile import NamedTemporaryFile
from textwrap import dedent

import pytest

from semgrep.config_resolver import Config
from semgrep.metrics import Metrics
from semgrep.metrics import MetricsState


@pytest.fixture(autouse=True)
def automocks(mocker) -> None:
    # this makes event_id deterministic
    mocker.patch("uuid.uuid4", return_value=uuid.UUID("0" * 32))


@pytest.fixture
def metrics(mocker) -> Metrics:
    return Metrics()


@pytest.mark.quick
@pytest.mark.parametrize(
    "first, second, is_equal",
    [
        (["p/r2c"], ["p/r2c"], True),
        (["p/r2c"], ["p/ci"], False),
        (["a", "b"], ["a", "b"], True),
        (["a", "b"], ["b", "a"], False),
    ],
)
@pytest.mark.freeze_time("2023-09-01 09:01:00")
def test_configs_hash(first, second, is_equal) -> None:
    first_metrics = Metrics()
    first_metrics.add_configs(first)
    second_metrics = Metrics()
    second_metrics.add_configs(second)

    # this provides better error messages than `(first_metrics == second_metrics) == is_equal`
    if is_equal:
        assert first_metrics == second_metrics
    else:
        assert first_metrics != second_metrics


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "first, second, is_equal",
    [
        ([0], [0], True),
        ([0], [1], False),
        ([0, 1, 2], [0, 1, 2], True),
        ([0, 1, 2], [2, 1, 0], True),
        ([0, 1, 2], [1], False),
    ],
)
@pytest.mark.freeze_time("2023-09-01 09:01:00")
def test_rules_hash(first, second, is_equal) -> None:
    config1 = dedent(
        """
        rules:
        - id: rule1
          pattern: $X == $X
          languages: [python]
          severity: INFO
          message: bad
        - id: rule2
          pattern: $X == $Y
          languages: [python]
          severity: INFO
          message: good
        - id: rule3
          pattern: $X < $Y
          languages: [c]
          severity: INFO
          message: doog
        """
    )
    # Load rules
    with NamedTemporaryFile() as tf1:
        tf1.write(config1.encode("utf-8"))
        tf1.flush()
        config, errors = Config.from_config_list([tf1.name], None)
        assert not errors
        rules = config.get_rules(True)
        assert len(rules) == 3

    first_rules = [rules[i] for i in first]
    second_rules = [rules[i] for i in second]

    first_metrics = Metrics()
    first_metrics.add_rules(first_rules, None)
    second_metrics = Metrics()
    second_metrics.add_rules(second_rules, None)

    # this provides better error messages than `(first_metrics == second_metrics) == is_equal`
    if is_equal:
        assert first_metrics == second_metrics
    else:
        assert first_metrics != second_metrics


class NetworkBlockedInTests(Exception):
    pass


@pytest.mark.quick
def test_send(metrics, mocker) -> None:
    """
    Check that no network does not cause failures
    """
    mocker.patch("socket.socket", side_effect=NetworkBlockedInTests)
    import requests

    # verify that network is blocked
    with pytest.raises(NetworkBlockedInTests):
        _ = requests.get("https://semgrep.dev", timeout=2)

    metrics.configure(MetricsState.ON)
    metrics.send()


@pytest.mark.quick
def test_project_hash(metrics):
    metrics.add_project_url("https://foo.bar.com/org/project.git")
    no_username_password = metrics.payload
    metrics.add_project_url("https://username:password@foo.bar.com/org/project.git")
    with_username_password_1 = metrics.payload
    metrics.add_project_url("https://username1:password2@foo.bar.com/org/project.git")
    with_username_password_2 = metrics.payload
    assert no_username_password == with_username_password_1
    assert with_username_password_1 == with_username_password_2
