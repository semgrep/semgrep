import json
from pathlib import Path
from textwrap import dedent

import pytest

import semgrep.output_from_core as core
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.config_resolver import parse_config_string
from semgrep.constants import RuleSeverity
from semgrep.dependency_aware_rule import SCA_FINDING_SCHEMA
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchSet


def create_rule() -> Rule:
    config = parse_config_string(
        "testfile",
        dedent(
            """
        rules:
        - id: rule_id
          pattern: $X == $X
          languages: [python]
          severity: INFO
          message: bad
        """
        ),
        None,
    )
    return Rule.from_yamltree(config["testfile"].value["rules"].value[0])


@pytest.mark.quick
def test_rule_match_attributes(mocker):
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    match = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("long.rule.id"),
            path=core.Fpath("relative/path/to/foo.py"),
            start=core.Position(3, 1, 24),
            end=core.Position(3, 15, 38),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    assert match.lines == ["    5 == 5 # nosem\n"], "wrong line was read from file"
    assert (
        match.previous_line == "def foo():\n"
    ), "wrong previous line was read from file"
    assert (
        match.syntactic_context == "5 == 5"
    ), "indent and comment must be removed from code"
    assert (
        match.syntactic_id == "e9f75ffe95edb9e7b898d5f8c475501a"
    ), "syntactic IDs must remain consistent to not trigger new notifications"


@pytest.mark.quick
def test_rule_match_sorting(mocker):
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
            6 == 6 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    line3 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(3, 1, 24),
            end=core.Position(3, 15, 38),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    line4 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(4, 1, 36),
            end=core.Position(4, 15, 50),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    # fmt: off
    assert (
        sorted([line4, line3]) == [line3, line4]
    ), "after sorting, matches on earlier lines must go first"
    # fmt: on


@pytest.mark.quick
def test_rule_match_hashing(mocker):
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    match = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(3, 1, 24),
            end=core.Position(3, 15, 38),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    assert {match, match} == {match}, "matches must deduplicate when added to a set"


@pytest.mark.quick
def test_rule_match_is_nosemgrep_agnostic(mocker):
    file_content = dedent(
        """
        # first line
        def foo():
            (5
                ==
            5)
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    match_1 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(3, 1, 28),
            end=core.Position(5, 2, 48),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    file_content = dedent(
        """
        # first line
        def foo():
            (5  # nosemgrep: something
                ==
            5)
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    match_2 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(3, 1, 28),
            end=core.Position(5, 2, 72),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    file_content = dedent(
        """
        # first line
        def foo():
            # nosemgrep: something
            (5
                ==
            5)
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    match_3 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(4, 1, 55),
            end=core.Position(6, 2, 75),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    assert (
        match_1.ci_unique_key == match_2.ci_unique_key
    ), "matches are identical per semgrep ci deduplication if the only difference is an inline nosemgrep comment"
    assert (
        match_1.ci_unique_key == match_3.ci_unique_key
    ), "matches are identical per semgrep ci deduplication if the only difference is a previous-line nosemgrep comment"


@pytest.mark.quick
def test_rule_match_set_indexes(mocker):
    rule = create_rule()
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
            5 == 5 # nosem
            6 == 6 # nosem
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    line3 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(3, 1, 24),
            end=core.Position(3, 15, 38),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    line4 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(4, 1, 36),
            end=core.Position(4, 15, 50),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    line5 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(5, 1, 48),
            end=core.Position(5, 15, 62),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    line6 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id"),
            path=core.Fpath("foo.py"),
            start=core.Position(6, 1, 60),
            end=core.Position(6, 15, 74),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )

    line7 = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule_id_wrong_one"),
            path=core.Fpath("foo.py"),
            start=core.Position(7, 1, 60),
            end=core.Position(7, 15, 74),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
    )
    matches = RuleMatchSet(rule)
    matches.update(
        [line3, line4, line5, line6]
    )  # we do need to add them in the correct order
    try:
        # Ensure we can't add rule matches with unassociated rule
        matches.update([line7])
        raise AssertionError()
    except ValueError:
        assert True
    sorted_matches = list(sorted(matches))
    assert sorted_matches[0].index == 0, "1st duplicate match must be assigned index 0"
    assert sorted_matches[1].index == 1, "2nd duplicate match must be assigned index 1"
    assert sorted_matches[3].index == 2, "3rd duplicate match must be assigned index 2"
    assert sorted_matches[2].index == 0, "unique match must be assigned index 0"


@pytest.mark.quick
def test_rule_match_to_app_finding(snapshot, mocker):
    mocker.patch.object(RuleMatch, "get_lines", lambda self: "foo()")
    dependency_match = out.DependencyMatch(
        dependency_pattern=out.DependencyPattern(
            ecosystem=out.Ecosystem(out.Pypi()),
            package="awscli",
            semver_range="== 1.11.82",
        ),
        found_dependency=out.FoundDependency(
            ecosystem=out.Ecosystem(out.Pypi()),
            package="awscli",
            version="1.11.82",
            resolved_url=None,
            allowed_hashes={
                "sha256": [
                    "149e90d6d8ac20db7a955ad60cf0e6881a3f20d37096140088356da6c716b0b1",
                    "ef6aaac3ca6cd92904cdd0d83f629a15f18053ec84e6432106f7a4d04ae4f5fb",
                ]
            },
            transitivity=out.Transitivity(out.Direct()),
        ),
        lockfile="foo/Pipfile.lock",
    )
    match = RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule.id"),
            path=core.Fpath("foo.py"),
            start=core.Position(0, 0, 0),
            end=core.Position(0, 0, 0),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
        extra={
            "sca_info": out.ScaInfo(
                sca_finding_schema=SCA_FINDING_SCHEMA,
                reachable=True,
                reachability_rule=True,
                dependency_match=dependency_match,
            )
        },
    )
    app_finding = match.to_app_finding_format("0")
    app_finding.commit_date = "1970-01-01T00:00:00"
    app_finding_str = (
        json.dumps(app_finding.to_json(), indent=2, sort_keys=True) + "\n"
    )  # Needed because pre-commit always adds a newline, seems weird
    snapshot.assert_match(app_finding_str, "results.json")


def create_sca_rule_match(sca_kind, reachable_in_code, transitivity):
    dependency_match = out.DependencyMatch(
        dependency_pattern=out.DependencyPattern(
            ecosystem=out.Ecosystem(out.Pypi()),
            package="awscli",
            semver_range="== 1.11.82",
        ),
        found_dependency=out.FoundDependency(
            ecosystem=out.Ecosystem(out.Pypi()),
            package="awscli",
            version="1.11.82",
            resolved_url=None,
            allowed_hashes={
                "sha256": [
                    "149e90d6d8ac20db7a955ad60cf0e6881a3f20d37096140088356da6c716b0b1",
                    "ef6aaac3ca6cd92904cdd0d83f629a15f18053ec84e6432106f7a4d04ae4f5fb",
                ]
            },
            transitivity=out.Transitivity(transitivity),
        ),
        lockfile="foo/Pipfile.lock",
    )
    return RuleMatch(
        message="message",
        metadata={"sca-kind": sca_kind, "dev.semgrep.actions": ["block"]},
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            check_id=core.RuleId("rule.id"),
            path=core.Fpath("foo.py"),
            start=core.Position(0, 0, 0),
            end=core.Position(0, 0, 0),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars({}), engine_kind=core.EngineKind(core.OSS())
            ),
        ),
        extra={
            "sca_info": out.ScaInfo(
                sca_finding_schema=SCA_FINDING_SCHEMA,
                reachable=reachable_in_code,
                reachability_rule=sca_kind == "reachable",
                dependency_match=dependency_match,
            )
        },
    )


@pytest.mark.quick
def test_supply_chain_blocking():
    assert create_sca_rule_match("reachable", True, out.Direct()).is_blocking
    assert create_sca_rule_match("reachable", True, out.Transitive()).is_blocking
    assert not create_sca_rule_match("reachable", False, out.Direct()).is_blocking
    assert not create_sca_rule_match("reachable", False, out.Transitive()).is_blocking
    assert create_sca_rule_match("upgrade-only", False, out.Direct()).is_blocking
    assert not create_sca_rule_match(
        "upgrade-only", False, out.Transitive()
    ).is_blocking
    assert create_sca_rule_match("upgrade-only", False, out.Unknown()).is_blocking
