from pathlib import Path
from textwrap import dedent

import pytest

import semgrep.output_from_core as core
from semgrep.config_resolver import parse_config_string
from semgrep.constants import RuleSeverity
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchSet


@pytest.fixture
def eqeq_rule() -> Rule:
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


@pytest.fixture
def double_eqeq_rule() -> Rule:
    config = parse_config_string(
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
    filepath="foo.py", start_line=3, end_line=3, rule_id="rule_id", metavars=None
) -> RuleMatch:
    return RuleMatch(
        message="message",
        severity=RuleSeverity.ERROR,
        match=core.CoreMatch(
            rule_id=core.RuleId(rule_id),
            location=core.Location(
                path=filepath,
                start=core.Position(start_line, 0, start_line * 5),
                end=core.Position(end_line, 5, end_line * 5 + 5),
            ),
            extra=core.CoreMatchExtra(
                metavars=core.Metavars(metavars if metavars else {}),
                engine_kind=core.EngineKind(core.OSS()),
            ),
        ),
        extra={"metavars": metavars if metavars else {}},
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
    matches = RuleMatchSet(eqeq_rule)
    matches.update([match_1, match_2])
    matches = list(sorted(matches))
    # Adding a RuleMatch to a RuleMatchSet does not update the index of the
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
    matches = RuleMatchSet(eqeq_rule)
    matches.update([match_1, match_2])
    matches = list(sorted(matches))
    # Adding a RuleMatch to a RuleMatchSet does not update the index of the
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
    matches = RuleMatchSet(double_eqeq_rule)
    matches.update([match_1, match_2])
    matches = list(sorted(matches))
    assert matches[0].start_line_hash != matches[0].end_line_hash
    assert matches[1].start_line_hash != matches[1].end_line_hash
    assert matches[0].start_line_hash == matches[1].end_line_hash
    assert matches[0].end_line_hash == matches[1].start_line_hash
