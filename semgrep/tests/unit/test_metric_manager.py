from pathlib import Path
from tempfile import NamedTemporaryFile
from textwrap import dedent
from unittest.mock import MagicMock
from unittest.mock import patch
from unittest.mock import PropertyMock

import pytest

from semgrep.config_resolver import Config
from semgrep.metric_manager import metric_manager
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times


def test_configs_hash() -> None:
    metric_manager.set_configs_hash(["p/r2c"])
    old = metric_manager._configs_hash
    metric_manager.set_configs_hash(["p/r2c"])
    assert metric_manager._configs_hash == old
    metric_manager.set_configs_hash(["not"])
    assert metric_manager._configs_hash != old

    metric_manager.set_configs_hash(["a", "b"])
    old = metric_manager._configs_hash
    metric_manager.set_configs_hash(["a", "b"])
    assert metric_manager._configs_hash == old
    metric_manager.set_configs_hash(["b", "a"])
    assert metric_manager._configs_hash != old


def test_rules_hash() -> None:
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
        config, errors = Config.from_config_list([tf1.name])
        assert not errors
        rules = config.get_rules(True)
        assert len(rules) == 3
        rule1, rule2, rule3 = rules

    metric_manager.set_rules_hash([rule1])
    old_hash = metric_manager._rules_hash
    metric_manager.set_rules_hash([rule1])
    assert old_hash == metric_manager._rules_hash

    metric_manager.set_rules_hash(rules)
    old_hash_2 = metric_manager._rules_hash
    metric_manager.set_rules_hash(rules)
    assert old_hash_2 == metric_manager._rules_hash

    assert old_hash != old_hash_2


def test_send() -> None:
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
        r = requests.get(
            "https://semgrep.dev",
            timeout=2,
        )

    metric_manager.enable()
    metric_manager.send()


def test_timings(snapshot) -> None:
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
        config, errors = Config.from_config_list([tf1.name])
        assert not errors
        rules = config.get_rules(True)
        assert len(rules) == 3
        rule1, rule2, rule3 = rules

    # Mock Path().stat().st_size
    with patch.object(Path, "stat") as stat_mock:
        m = MagicMock()
        # Note this mock is a little fragile and assumes st_size is called twice
        # once in set_run_times then once in set_run_timings and assumes that
        # it will be called for target[0] then target[1] then target[0] then target[1]
        type(m).st_size = PropertyMock(side_effect=[1, 2, 1, 2])
        stat_mock.return_value = m

        targets = [Path("a"), Path("b")]

        profiling_data = ProfilingData()
        profiling_data.set_run_times(
            rule1,
            targets[0],
            Times(match_time=0.2, run_time=0.4, parse_time=0.3),
        )
        profiling_data.set_run_times(
            rule2,
            targets[1],
            Times(match_time=1.2, run_time=1.4, parse_time=0.2),
        )
        profiling_data.set_rule_parse_time(rule1, 0.05)
        profiling_data.set_rule_parse_time(rule2, 0.04)

        metric_manager.set_run_timings(profiling_data, targets, rules)

    assert metric_manager._rule_stats == [
        {
            "ruleHash": "720c14cd416c021bc45d6db0689dd0eb54d1d062bf9f446f85dae0cb5d1438c0",
            "parseTime": 0.05,
            "matchTime": 0.2,
            "runTime": 0.4,
            "bytesScanned": 1,
        },
        {
            "ruleHash": "a5360bb56a3b0a3c33c1bb2b6e7d6465e9a246ccb8940bc05710bc5b35a43e30",
            "parseTime": 0.04,
            "matchTime": 1.2,
            "runTime": 1.4,
            "bytesScanned": 2,
        },
        {
            "ruleHash": "2cc5dbc0cae3a8b6af0d8792079251c4d861b5e16815c1b1cdba676d1c96c5a5",
            "parseTime": 0.0,
            "matchTime": None,
            "runTime": None,
            "bytesScanned": 0,
        },
    ]
    assert metric_manager._file_stats == [
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


def test_project_hash():
    metric_manager.set_project_hash("https://foo.bar.com/org/project.git")
    no_username_password = metric_manager._project_hash
    metric_manager.set_project_hash(
        "https://username:password@foo.bar.com/org/project.git"
    )
    with_username_password_1 = metric_manager._project_hash
    metric_manager.set_project_hash(
        "https://username1:password2@foo.bar.com/org/project.git"
    )
    with_username_password_2 = metric_manager._project_hash
    assert no_username_password == with_username_password_1
    assert with_username_password_1 == with_username_password_2
