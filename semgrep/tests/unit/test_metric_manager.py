from tempfile import NamedTemporaryFile
from textwrap import dedent

import pytest

from semgrep.config_resolver import Config
from semgrep.metric_manager import metric_manager


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
