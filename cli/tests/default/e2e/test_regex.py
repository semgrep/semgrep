import pytest
from tests.conftest import _clean_stdout
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_regex_rule__top(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex/regex-top.yaml").stdout, "results.json"
    )


@pytest.mark.kinda_slow
def test_regex_rule__utf8(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-utf8.yaml", target_name="basic/regex-utf8.txt"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__utf8_on_image(run_semgrep_in_tmp: RunSemgrep, snapshot):
    # https://github.com/returntocorp/semgrep/issues/4258
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-utf8.yaml", target_name="image/semgrep.png"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__child(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex/regex-child.yaml").stdout, "results.json"
    )


@pytest.mark.kinda_slow
def test_regex_rule__not(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not.yaml", target_name="basic/stupid.py"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__not2(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not2.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__pattern_regex_and_pattern_not_regex(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not-with-pattern-regex.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__issue2465(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/issue2465.yaml",
            target_name="pattern-not-regex/issue2465.requirements.txt",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_regex_rule__invalid_expression(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/regex/regex-invalid.yaml", assert_exit_code=2
    )
    snapshot.assert_match(stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(stdout), "error.json")


# https://github.com/returntocorp/semgrep/pull/8510
@pytest.mark.kinda_slow
def test_metavariable_regex_const_prop(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-regex/metavariable-regex-const-prop.yaml",
            target_name="metavariable_propagation/metavariable-regex-const-prop.dockerfile",
        ).stdout,
        "results.json",
    )


# https://github.com/returntocorp/semgrep/pull/8510
@pytest.mark.kinda_slow
def test_metavariable_regex_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex/metavariable-regex.yaml").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_regex_multi_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-regex/metavariable-regex-multi-rule.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_multi_regex_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-regex/metavariable-regex-multi-regex.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_with_any_language_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-any-language.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_with_any_language_multiple_rule(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-any-language-multiple.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_invalid_regex_with_any_language_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/regex/regex-any-language-invalid.yaml",
        target_name="basic/regex-any-language.html",
        assert_exit_code=7,
    )
    snapshot.assert_match(stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(stdout), "error.json")


@pytest.mark.kinda_slow
def test_regex_with_any_language_rule_none_alias(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-any-language-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_with_any_language_multiple_rule_none_alias(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-any-language-multiple-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


# https://github.com/returntocorp/semgrep/pull/8510
@pytest.mark.kinda_slow
def test_metavariable_propagation_regex(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-regex-propagation.yaml",
            target_name="metavariable_propagation/metavariable-regex-propagation.py",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_pattern_regex_empty_file(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-regex-empty-file.yaml",
            target_name="empty/totally_empty_file",
        ).stdout,
        "results.json",
    )
