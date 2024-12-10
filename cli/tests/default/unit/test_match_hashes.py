from pathlib import Path
from textwrap import dedent

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.config_resolver import parse_config_string
from semgrep.dependency_aware_rule import SCA_FINDING_SCHEMA
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatches
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyMatch
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity


@pytest.fixture
def eqeq_rule() -> Rule:
    config, config_errors = parse_config_string(
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


@pytest.fixture
def double_eqeq_rule() -> Rule:
    config, config_errors = parse_config_string(
        "testfile",
        dedent(
            """
        rules:
        - id: rule_id
          pattern: |
            $X == $X
            $Y == $Y
          languages: [python]
          severity: INFO
          message: bad
        """
        ),
        None,
    )
    return Rule.from_yamltree(config["testfile"].value["rules"].value[0])


@pytest.fixture
def lockfile_only_rule() -> Rule:
    config, config_errors = parse_config_string(
        "testfile",
        dedent(
            """
        rules:
        - id: rule_id
          r2c-internal-project-depends-on:
              package: "foo"
              version: ">=1.0.0"
              namespace: "pypi"
          languages: [python]
          severity: INFO
          message: bad
        """
        ),
        None,
    )
    return Rule.from_yamltree(config["testfile"].value["rules"].value[0])


@pytest.fixture
def foo_contents() -> str:
    return dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
            5 == 5 # nosem
            6 == 6 # nosem
            5 == 5 # nosem
        """
    ).lstrip()


def get_rule_match(
    filepath="foo.py",
    start_line=3,
    end_line=3,
    rule_id="rule_id",
    metavars=None,
    metadata=None,
) -> RuleMatch:
    return RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId(rule_id),
            path=out.Fpath(filepath),
            start=out.Position(start_line, 0, start_line * 5),
            end=out.Position(end_line, 5, end_line * 5 + 5),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars(metavars if metavars else {}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
        extra={"metavars": metavars if metavars else {}},
        metadata=metadata if metadata else {},
    )


def get_lockfile_only_rule_match(
    filepath="requirements.txt",
    start_line=3,
    end_line=3,
    rule_id="rule_id",
) -> RuleMatch:
    return RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId(rule_id),
            path=out.Fpath(filepath),
            start=out.Position(start_line, 0, 0),
            end=out.Position(end_line, 0, 0),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                engine_kind=out.EngineKind(out.OSS()),
                is_ignored=False,
            ),
        ),
        extra={
            "sca_info": out.ScaMatch(
                reachable=False,
                reachability_rule=False,
                sca_finding_schema=SCA_FINDING_SCHEMA,
                dependency_match=DependencyMatch(
                    dependency_pattern=out.ScaPattern(
                        ecosystem=Ecosystem(Pypi()),
                        package="foo",
                        semver_range=">=1.0.0",
                    ),
                    found_dependency=FoundDependency(
                        ecosystem=Ecosystem(Pypi()),
                        package="foo",
                        version="1.0.0",
                        allowed_hashes={},
                        transitivity=Transitivity(Transitive()),
                    ),
                    lockfile=filepath,
                ),
            )
        },
        metadata={},
    )


@pytest.mark.quick
def test_code_hash_independent_of_filepath(mocker, foo_contents):
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=foo_contents))
    match_1 = get_rule_match(filepath="foo.py")
    match_2 = get_rule_match(filepath="bar/foo.py")
    assert match_1.syntactic_id != match_2.syntactic_id
    assert match_1.match_based_id != match_2.match_based_id
    assert match_1.code_hash == match_2.code_hash
    assert match_1.pattern_hash == match_2.pattern_hash


@pytest.mark.quick
def test_code_hash_independent_of_rulename(mocker, foo_contents):
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=foo_contents))
    match_1 = get_rule_match(rule_id="first.rule.id")
    match_2 = get_rule_match(rule_id="second.rule.id")
    assert match_1.syntactic_id != match_2.syntactic_id
    assert match_1.match_based_id != match_2.match_based_id
    assert match_1.code_hash == match_2.code_hash
    assert match_1.pattern_hash == match_2.pattern_hash


@pytest.mark.quick
def test_code_hash_independent_of_index(mocker, eqeq_rule, foo_contents):
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=foo_contents))
    match_1 = get_rule_match(start_line=3, end_line=3)
    match_2 = get_rule_match(start_line=4, end_line=4)
    matches = RuleMatches(eqeq_rule)
    matches.update([match_1, match_2])
    matches = list(sorted(matches))
    # Adding a RuleMatch to a RuleMatches does not update the index of the
    # original RuleMatch object, instead it creates a copy of each RuleMatch
    # that gets added to the set, and updates those indexes
    assert matches[0].index == 0
    assert matches[1].index == 1
    assert matches[0].syntactic_id != matches[1].syntactic_id
    assert matches[0].match_based_id != matches[1].match_based_id
    assert matches[0].code_hash == matches[1].code_hash
    assert matches[0].pattern_hash == matches[1].pattern_hash


@pytest.mark.quick
def test_code_hash_changes_with_code(mocker, eqeq_rule, foo_contents):
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=foo_contents))
    match_1 = get_rule_match(
        start_line=3, end_line=3, metavars={"$X": {"abstract_content": "5"}}
    )
    match_2 = get_rule_match(
        start_line=5, end_line=5, metavars={"$X": {"abstract_content": "6"}}
    )
    matches = RuleMatches(eqeq_rule)
    matches.update([match_1, match_2])
    matches = list(sorted(matches))
    # Adding a RuleMatch to a RuleMatches does not update the index of the
    # original RuleMatch object, instead it creates a copy of each RuleMatch
    # that gets added to the set, and updates those indexes
    assert matches[0].index == 0
    assert matches[1].index == 0
    assert matches[0].code_hash != matches[1].code_hash
    assert matches[0].pattern_hash != matches[1].pattern_hash


@pytest.mark.quick
def test_line_hashes_hash_correct_line(mocker, double_eqeq_rule, foo_contents):
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=foo_contents))
    match_1 = get_rule_match(start_line=4, end_line=5)  # 5 == 5\n6 == 6\n
    match_2 = get_rule_match(start_line=5, end_line=6)  # 6 == 6\n5 == 5\n
    matches = RuleMatches(double_eqeq_rule)
    matches.update([match_1, match_2])
    matches = list(sorted(matches))
    assert matches[0].start_line_hash != matches[0].end_line_hash
    assert matches[1].start_line_hash != matches[1].end_line_hash
    assert matches[0].start_line_hash == matches[1].end_line_hash
    assert matches[0].end_line_hash == matches[1].start_line_hash


@pytest.mark.quick
def test_same_code_hash_for_previous_scan_finding(mocker, foo_contents):
    """
    For the reliable fixed status work, we start sending rules run during the previous
    scan too.

    As the engine can't process two rules with same rule.id, we override the rule.id for
    previous scan findings with something unique. However, we store the original rule.id
    in the metadata.

    Before computing the match_based_id, we fetch the rule.id from the metadata and use
    it to compute the match_based_id.

    This test ensures that the match_based_id for the previous scan finding is same as
    the match_based_id for the current scan finding.
    """
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=foo_contents))
    curr_scan_metadata = {
        "semgrep.dev": {
            "rule": {
                "rule_id": "rule_id",
                "version_id": "version1",
                "url": "https://semgrep.dev/r/python.eqeq-five",
                "shortlink": "https://sg.run/abcd",
            },
            "src": "unchanged",
        }
    }
    prev_scan_metadata = {
        "semgrep.dev": {
            "rule": {
                "rule_id": "rule_idversion1",
                "version_id": "version1",
                "url": "https://semgrep.dev/r/python.eqeq-five",
                "shortlink": "https://sg.run/abcd",
                "rule_name": "rule_id",
            },
            "src": "previous-scan",
        }
    }
    curr_scan_match = get_rule_match(
        start_line=3, end_line=3, metadata=curr_scan_metadata, rule_id="rule_id"
    )
    prev_scan_match = get_rule_match(
        start_line=3, end_line=3, metadata=prev_scan_metadata, rule_id="rule_idversion1"
    )
    assert curr_scan_match.syntactic_id == prev_scan_match.syntactic_id
    assert curr_scan_match.match_based_id == prev_scan_match.match_based_id
    assert curr_scan_match.code_hash == prev_scan_match.code_hash
    assert prev_scan_match.pattern_hash == prev_scan_match.pattern_hash


# Same rule finding the same package at two different lines, the match_based_ids should be different
@pytest.mark.quick
def test_lockfile_only(mocker, lockfile_only_rule):
    lockfile_contents = dedent(
        """
        foo == 1.0.0
        foo == 2.0.0
        foo == 3.0.0
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=lockfile_contents))
    match1 = get_lockfile_only_rule_match(start_line=1, end_line=1)
    match2 = get_lockfile_only_rule_match(start_line=2, end_line=2)
    matches = RuleMatches(lockfile_only_rule)
    matches.update([match1, match2])
    matches = list(sorted(matches))
    assert matches[0].match_based_id != matches[1].match_based_id
