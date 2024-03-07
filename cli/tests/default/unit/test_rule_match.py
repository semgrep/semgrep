import json
from pathlib import Path
from textwrap import dedent
from typing import Union

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.config_resolver import parse_config_string
from semgrep.dependency_aware_rule import SCA_FINDING_SCHEMA
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatches


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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("long.rule.id"),
            path=out.Fpath("relative/path/to/foo.py"),
            start=out.Position(3, 1, 24),
            end=out.Position(3, 15, 38),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(3, 1, 24),
            end=out.Position(3, 15, 38),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )
    line4 = RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(4, 1, 36),
            end=out.Position(4, 15, 50),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )
    # fmt: off
    assert (
        sorted([line4, line3]) == [line3, line4]
    ), "after sorting, matches on earlier lines must go first"
    # fmt: on


@pytest.mark.quick
def test_rule_match_sorting_with_git_info(mocker):
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(3, 1, 24),
            end=out.Position(3, 15, 38),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )
    line4 = RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("/tmp/fakepath.py"),
            start=out.Position(4, 1, 36),
            end=out.Position(4, 15, 50),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
                historical_info=out.HistoricalInfo(
                    git_blob=out.Sha1("d7a45f0ee770d69753179824d1c828557ce19054"),
                    git_commit=out.Sha1("d7a45f0ee770d69753179824d1c828557ce19054"),
                    git_commit_timestamp=out.Datetime("2024-03-07T20:11:35Z"),
                ),
            ),
        ),
    )
    # Should not raise; the values should be comparable without typing issues.
    [line4, line3].sort()


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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(3, 1, 24),
            end=out.Position(3, 15, 38),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(3, 1, 28),
            end=out.Position(5, 2, 48),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(3, 1, 28),
            end=out.Position(5, 2, 72),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(4, 1, 55),
            end=out.Position(6, 2, 75),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(3, 1, 24),
            end=out.Position(3, 15, 38),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )
    line4 = RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(4, 1, 36),
            end=out.Position(4, 15, 50),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )
    line5 = RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(5, 1, 48),
            end=out.Position(5, 15, 62),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )
    line6 = RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id"),
            path=out.Fpath("foo.py"),
            start=out.Position(6, 1, 60),
            end=out.Position(6, 15, 74),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )

    line7 = RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule_id_wrong_one"),
            path=out.Fpath("foo.py"),
            start=out.Position(7, 1, 60),
            end=out.Position(7, 15, 74),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
    )
    matches = RuleMatches(rule)
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule.id"),
            path=out.Fpath("foo.py"),
            start=out.Position(0, 0, 0),
            end=out.Position(0, 0, 0),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
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
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule.id"),
            path=out.Fpath("foo.py"),
            start=out.Position(0, 0, 0),
            end=out.Position(0, 0, 0),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
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


def create_validator_rule_match(
    validation_state_actions: dict,
    match_validation_state: Union[
        out.ConfirmedValid, out.ConfirmedInvalid, out.ValidationError, out.NoValidator
    ],
):
    return RuleMatch(
        message="message",
        metadata={
            "dev.semgrep.actions": ["monitor"],
            "dev.semgrep.validation_state.actions": validation_state_actions,
        },
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule.id"),
            path=out.Fpath("foo.py"),
            start=out.Position(0, 0, 0),
            end=out.Position(0, 0, 0),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.PRO()),
                is_ignored=False,
                validation_state=out.ValidationState(match_validation_state),
            ),
        ),
    )


@pytest.mark.quick
@pytest.mark.parametrize(
    ("validation_state_actions", "match_validation_state", "is_blocking"),
    [
        (
            {"valid": "comment", "invalid": "monitor", "error": "block"},
            out.ConfirmedValid(),
            False,
        ),
        (
            {"valid": "comment", "invalid": "monitor", "error": "block"},
            out.ConfirmedInvalid(),
            False,
        ),
        (
            {"valid": "comment", "invalid": "monitor", "error": "block"},
            out.ValidationError(),
            True,
        ),
        (
            {"valid": "comment", "invalid": "monitor", "error": "block"},
            out.NoValidator(),
            False,
        ),
        (
            {"valid": "block", "invalid": "block", "error": "block"},
            out.ConfirmedValid(),
            True,
        ),
        (
            {"valid": "block", "invalid": "block", "error": "block"},
            out.ConfirmedInvalid(),
            True,
        ),
        (
            {"valid": "block", "invalid": "block", "error": "block"},
            out.NoValidator(),
            True,
        ),
    ],
)
def test_validator_rule_blocking(
    validation_state_actions, match_validation_state, is_blocking
):
    rule_match = create_validator_rule_match(
        validation_state_actions, match_validation_state
    )
    assert rule_match.is_blocking == is_blocking
