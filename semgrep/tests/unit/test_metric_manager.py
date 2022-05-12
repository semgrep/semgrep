from pathlib import Path
from tempfile import NamedTemporaryFile
from textwrap import dedent

import pytest

from semgrep.config_resolver import Config
from semgrep.metrics import Metrics
from semgrep.metrics import MetricsState
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times


@pytest.fixture
def metrics() -> Metrics:
    return Metrics()


@pytest.mark.quick
def test_configs_hash(metrics) -> None:
    metrics.set_configs_hash(["p/r2c"])
    old = metrics._configs_hash
    metrics.set_configs_hash(["p/r2c"])
    assert metrics._configs_hash == old
    metrics.set_configs_hash(["not"])
    assert metrics._configs_hash != old

    metrics.set_configs_hash(["a", "b"])
    old = metrics._configs_hash
    metrics.set_configs_hash(["a", "b"])
    assert metrics._configs_hash == old
    metrics.set_configs_hash(["b", "a"])
    assert metrics._configs_hash != old


@pytest.mark.quick
def test_rules_hash(metrics) -> None:
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
        rule1, rule2, rule3 = rules

    metrics.set_rules_hash([rule1])
    old_hash = metrics._rules_hash
    metrics.set_rules_hash([rule1])
    assert old_hash == metrics._rules_hash

    metrics.set_rules_hash(rules)
    old_hash_2 = metrics._rules_hash
    metrics.set_rules_hash(rules)
    assert old_hash_2 == metrics._rules_hash

    assert old_hash != old_hash_2


@pytest.mark.quick
def test_send(metrics) -> None:
    """
    Check that no network does not cause failures
    """
    import socket
    import requests

    class block_network(socket.socket):
        def __init__(self, *args, **kwargs):
            raise Exception("Network call blocked")

    socket.socket = block_network  # type: ignore

    # test that network is blocked
    with pytest.raises(Exception):
        _ = requests.get(
            "https://semgrep.dev",
            timeout=2,
        )

    metrics.configure(MetricsState.ON, None)
    metrics.send()


@pytest.mark.quick
def test_timings(metrics, mocker) -> None:
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
        rule1, rule2, rule3 = rules

    # Mock Path().stat().st_size
    mock_stat_result = mocker.MagicMock()
    type(mock_stat_result).st_size = mocker.PropertyMock(side_effect=[1, 2, 1, 2])
    mocker.patch.object(Path, "stat", return_value=mock_stat_result)
    # Note this mock is a little fragile and assumes st_size is called twice
    # once in set_file_times then once in set_run_timings and assumes that
    # it will be called for target[0] then target[1] then target[0] then target[1]

    targets = [Path("a"), Path("b")]

    profiling_data = ProfilingData()

    target_a_time = {
        rule1.id2: Times(match_time=0.2, parse_time=0.3),
    }
    target_b_time = {
        rule2.id2: Times(match_time=1.2, parse_time=0.2),
    }
    profiling_data.set_file_times(targets[0], target_a_time, 0.4)
    profiling_data.set_file_times(targets[1], target_b_time, 1.4)
    profiling_data.set_rules_parse_time(0.09)

    metrics.set_run_timings(profiling_data, targets, rules)

    assert metrics._rule_stats == [
        {
            "ruleHash": "720c14cd416c021bc45d6db0689dd0eb54d1d062bf9f446f85dae0cb5d1438c0",
            "matchTime": 0.2,
            "bytesScanned": 1,
        },
        {
            "ruleHash": "a5360bb56a3b0a3c33c1bb2b6e7d6465e9a246ccb8940bc05710bc5b35a43e30",
            "matchTime": 1.2,
            "bytesScanned": 2,
        },
        {
            "ruleHash": "2cc5dbc0cae3a8b6af0d8792079251c4d861b5e16815c1b1cdba676d1c96c5a5",
            "matchTime": None,
            "bytesScanned": 0,
        },
    ]
    assert metrics._file_stats == [
        {
            "size": 1,
            "numTimesScanned": 1,
            "parseTime": 0.3,
            "matchTime": 0.2,
            "runTime": 0.4,
        },
        {
            "size": 2,
            "numTimesScanned": 1,
            "parseTime": 0.2,
            "matchTime": 1.2,
            "runTime": 1.4,
        },
    ]


@pytest.mark.quick
def test_project_hash(metrics):
    metrics.set_project_hash("https://foo.bar.com/org/project.git")
    no_username_password = metrics._project_hash
    metrics.set_project_hash("https://username:password@foo.bar.com/org/project.git")
    with_username_password_1 = metrics._project_hash
    metrics.set_project_hash("https://username1:password2@foo.bar.com/org/project.git")
    with_username_password_2 = metrics._project_hash
    assert no_username_password == with_username_password_1
    assert with_username_password_1 == with_username_password_2
