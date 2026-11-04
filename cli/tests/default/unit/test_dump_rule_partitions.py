#
# Copyright (c) 2026 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
"""Unit tests for ``dump_partitions_and_exit`` in ``run_scan``."""
from typing import Any
from typing import Dict
from typing import List

import pytest

import semgrep.run_scan as run_scan
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.rule import Rule


def _sast_rule(rule_id: str) -> Dict[str, Any]:
    return {
        "id": rule_id,
        "message": "m",
        "languages": ["python"],
        "severity": "ERROR",
        "pattern": "foo()",
    }


def _sca_rule(rule_id: str) -> Dict[str, Any]:
    # An SCA rule as it can appear in a Code policy: no patterns, only a
    # dependency condition.
    return {
        "id": rule_id,
        "message": "m",
        "languages": ["js"],
        "severity": "ERROR",
        "r2c-internal-project-depends-on": {
            "namespace": "npm",
            "package": "nx",
            "version": "== 20.9.0",
        },
    }


def _dump(
    monkeypatch: pytest.MonkeyPatch, raw_rules: List[Dict[str, Any]]
) -> List[str]:
    """Run dump_partitions_and_exit with the RPC stubbed out; return the ids
    of the rules that would be sent to be partitioned."""
    sent: List[out.DumpRulePartitionsParams] = []

    def fake_dump(params: out.DumpRulePartitionsParams) -> bool:
        sent.append(params)
        return True

    monkeypatch.setattr(run_scan, "dump_rule_partitions", fake_dump)
    params = out.DumpRulePartitionsParams(
        rules=out.RawJson([]),
        n_partitions=2,
        output_dir=out.Fpath("unused"),
    )
    rules = [Rule.from_json(r) for r in raw_rules]
    with pytest.raises(SystemExit) as exit_info:
        run_scan.dump_partitions_and_exit(rules, params)
    assert exit_info.value.code == 0
    assert len(sent) == 1
    return [r["id"] for r in sent[0].rules.value["rules"]]


@pytest.mark.quick
def test_sca_rules_are_skipped_with_a_warning(
    monkeypatch: pytest.MonkeyPatch, caplog: pytest.LogCaptureFixture
) -> None:
    ids = _dump(
        monkeypatch,
        [_sast_rule("a"), _sca_rule("sca-1"), _sast_rule("b"), _sca_rule("sca-2")],
    )
    assert ids == ["a", "b"]
    warnings = [r.getMessage() for r in caplog.records if r.levelname == "WARNING"]
    assert len(warnings) == 1
    assert "2 Supply Chain rule(s)" in warnings[0]
    # Only the rule ids are logged, never the rule bodies.
    assert "sca-1" in warnings[0] and "sca-2" in warnings[0]
    assert "r2c-internal-project-depends-on" not in warnings[0]
    assert "nx" not in warnings[0]


@pytest.mark.quick
def test_no_warning_without_sca_rules(
    monkeypatch: pytest.MonkeyPatch, caplog: pytest.LogCaptureFixture
) -> None:
    ids = _dump(monkeypatch, [_sast_rule("a"), _sast_rule("b")])
    assert ids == ["a", "b"]
    assert not [r for r in caplog.records if r.levelname == "WARNING"]
