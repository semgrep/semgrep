import collections
import json
from pathlib import Path
from subprocess import CalledProcessError
from typing import Dict
from xml.etree import cElementTree

import pytest

from semgrep import __VERSION__
from semgrep.constants import OutputFormat

GITHUB_TEST_GIST_URL = (
    "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"
)


# https://stackoverflow.com/a/10077069
def etree_to_dict(t):
    """
    A simple and sufficient XML -> dict conversion function. This function is
    used to perform basic XML test data comparisons.
    """
    d: Dict[str, Dict] = {t.tag: {}}
    children = list(t)
    if children:
        dd = collections.defaultdict(list)
        for dc in map(etree_to_dict, children):
            for k, v in dc.items():
                dd[k].append(v)
        d = {t.tag: {k: v[0] if len(v) == 1 else v for k, v in dd.items()}}
    if t.attrib:
        d[t.tag].update(("@" + k, v) for k, v in t.attrib.items())
    if t.text:
        text = t.text.strip()
        if children or t.attrib:
            if text:
                d[t.tag]["#text"] = text
        else:
            d[t.tag] = text
    return d


def clean_sarif_output(output):
    # Rules are logically a set so the JSON list's order doesn't matter
    # we make the order deterministic here so that snapshots match across runs
    # the proper solution will be https://github.com/joseph-roitman/pytest-snapshot/issues/14
    output["runs"][0]["tool"]["driver"]["rules"] = sorted(
        output["runs"][0]["tool"]["driver"]["rules"],
        key=lambda rule: str(rule["id"]),
    )

    # Semgrep version is included in sarif output. Verify this independently so
    # snapshot does not need to be updated on version bump
    assert output["runs"][0]["tool"]["driver"]["semanticVersion"] == __VERSION__
    output["runs"][0]["tool"]["driver"]["semanticVersion"] = "placeholder"

    return output


def test_basic_rule__local(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/eqeq.yaml")[0], "results.json")


def test_basic_rule__relative(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/../rules/eqeq.yaml")[0],
        "results.json",
    )


def test_noextension_filtering(run_semgrep_in_tmp, snapshot):
    """
    Check that semgrep does not filter out files without extensions when
    said file is explicitly passed
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml", target_name="basic/stupid_no_extension"
        )[0],
        "results.json",
    )


def test_noextension_filtering_optimizations(run_semgrep_in_tmp, snapshot):
    """
    Check that semgrep does not filter out files without extensions when
    said file is explicitly passed
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml",
            target_name="basic/stupid_no_extension",
            options=["--optimizations", "all"],
        )[0],
        "results.json",
    )


def test_basic_rule__absolute(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(Path.cwd() / "rules" / "eqeq.yaml")[0],
        "results.json",
    )


def test_terminal_output(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq.yaml", output_format=OutputFormat.TEXT)[0],
        "output.txt",
    )


def test_multiline(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq.yaml", target_name="multiline")[0],
        "results.json",
    )


def test_junit_xml_output(run_semgrep_in_tmp, snapshot):
    output, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml", output_format=OutputFormat.JUNIT_XML
    )
    result = etree_to_dict(cElementTree.XML(output))

    filename = snapshot.snapshot_dir / "results.xml"
    expected = etree_to_dict(cElementTree.XML(filename.read_text()))

    assert expected == result


def test_sarif_output(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq.yaml", output_format=OutputFormat.SARIF)[0]
    )

    sarif_output = clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )


# If there are nosemgrep comments to ignore findings, SARIF output should include them
# labeled as suppressed.
def test_sarif_output_include_nosemgrep(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp(
            "rules/regex-nosemgrep.yaml",
            target_name="basic/regex-nosemgrep.txt",
            output_format=OutputFormat.SARIF,
        )[0]
    )

    sarif_output = clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )


def test_sarif_output_with_source(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq-source.yml", output_format=OutputFormat.SARIF)[0]
    )

    sarif_output = clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )

    # Assert that each sarif rule object has a helpURI
    for rule in sarif_output["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("helpUri", None) is not None


def test_sarif_output_with_source_edit(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq-meta.yaml", output_format=OutputFormat.SARIF)[0]
    )

    sarif_output = clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )

    # Assert that each sarif rule object has a helpURI
    for rule in sarif_output["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("help", None) is not None


def test_sarif_output_with_nosemgrep_and_error(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="nosemgrep/eqeq-nosemgrep.py",
            output_format=OutputFormat.SARIF,
            options=["--error"],
        )[0]
    )

    sarif_output = clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )


def test_url_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(GITHUB_TEST_GIST_URL)[0],
        "results.json",
    )


def test_registry_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("r2c")[0],
        "results.json",
    )


def test_hidden_rule__explicit(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/hidden/.hidden")[0], "results.json")


def test_hidden_rule__implicit(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/hidden")[0]
    assert excinfo.value.returncode == 7
    snapshot.assert_match(excinfo.value.stdout, "error.json")

    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/hidden", output_format=OutputFormat.TEXT)[0]
    assert excinfo.value.returncode == 7
    snapshot.assert_match(excinfo.value.stderr, "error.txt")


def test_default_rule__file(run_semgrep_in_tmp, snapshot):
    Path(".semgrep.yml").symlink_to(Path("rules/eqeq.yaml").resolve())
    snapshot.assert_match(run_semgrep_in_tmp()[0], "results.json")


def test_default_rule__folder(run_semgrep_in_tmp, snapshot):
    Path(".semgrep").mkdir()
    Path(".semgrep/.semgrep.yml").symlink_to(Path("rules/eqeq.yaml").resolve())

    snapshot.assert_match(run_semgrep_in_tmp()[0], "results.json")


def test_regex_rule__top(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/regex-top.yaml")[0], "results.json")


def test_regex_rule__child(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex-child.yaml")[0], "results.json"
    )


def test_regex_rule__not(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not.yaml", target_name="basic/stupid.py"
        )[0],
        "results.json",
    )


def test_regex_rule__not2(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not2.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_regex_rule__pattern_regex_and_pattern_not_regex(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not-with-pattern-regex.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_regex_rule__issue2465(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/issue2465.yaml",
            target_name="pattern-not-regex/issue2465.requirements.txt",
        )[0],
        "results.json",
    )


def test_regex_rule__invalid_expression(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/regex-invalid.yaml")[0]
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_regex_rule__nosemgrep(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-nosemgrep.yaml", target_name="basic/regex-nosemgrep.txt"
        )[0],
        "results.json",
    )


def test_nested_patterns_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-patterns.yaml")[0], "results.json"
    )


def test_nested_pattern_either_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-pattern-either.yaml")[0], "results.json"
    )


def test_nosem_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/nosem.yaml")[0], "results.json")


def test_nosem_rule_unicode(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/nosem-unicode.yaml", target_name="advanced_nosem/nosem-unicode.py"
        )[0],
        "results.json",
    )


def test_nosem_rule__invalid_id(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/nosem.yaml", target_name="nosem_invalid_id")[0]
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_nosem_rule__with_disable_nosem(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nosem.yaml", options=["--disable-nosem"])[0],
        "results.json",
    )


def test_metavariable_regex_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex.yaml")[0], "results.json"
    )


def test_metavariable_regex_multi_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-rule.yaml")[0],
        "results.json",
    )


def test_metavariable_multi_regex_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-regex.yaml")[0],
        "results.json",
    )


def test_regex_with_any_language_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language.yaml", target_name="basic/regex-any-language.html"
        )[0],
        "results.json",
    )


def test_regex_with_any_language_multiple_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-multiple.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_invalid_regex_with_any_language_rule(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            "rules/regex-any-language-invalid.yaml",
            target_name="basic/regex-any-language.html",
        )
    assert excinfo.value.returncode not in (0, 1)
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_regex_with_any_language_rule_none_alias(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_regex_with_any_language_multiple_rule_none_alias(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-multiple-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_timeout(run_semgrep_in_tmp, snapshot):
    # Check that semgrep-core timeouts are properly handled

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            options=["--timeout", "1"],
            target_name="equivalence",
            strict=False,
        )[0],
        "results.json",
    )


def test_spacegrep_timeout(run_semgrep_in_tmp, snapshot):
    # Check that spacegrep timeouts are handled gracefully.
    #
    # The pattern is designed to defeat any optimization that would
    # prevent a timeout. Both the words 'Frob' and 'Yoyodyne' occur
    # once in the file but in a different order, preventing any match.
    #
    pattern = "$A ... $B ... $C ... Frob ... Yoyodyne"

    stdout, stderr = run_semgrep_in_tmp(
        config=None,
        target_name="spacegrep_timeout/gnu-lgplv2.txt",
        options=[
            "--lang",
            "generic",
            "--pattern",
            pattern,
            "--timeout",
            "1",
        ],
        output_format=OutputFormat.TEXT,
        strict=False,  # don't fail due to timeout
    )

    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")


def test_max_memory(run_semgrep_in_tmp, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/long.yaml",
        options=["--verbose", "--max-memory", "1"],
        target_name="equivalence",
        strict=False,
    )
    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")


def test_timeout_threshold(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
        )[0],
        "results.json",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
        )[1],
        "error.txt",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "2"],
            target_name="equivalence",
            strict=False,
        )[1],
        "error_2.txt",
    )


def test_metavariable_comparison_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison.yaml")[0], "results.json"
    )


def test_metavariable_comparison_rule_base(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-base.yaml")[0], "results.json"
    )


def test_metavariable_comparison_rule_strip(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-strip.yaml")[0],
        "results.json",
    )


def test_metavariable_comparison_rule_bad_content(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-bad-content.yaml")[0],
        "results.json",
    )


def test_multiple_configs_file(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", "rules/eqeq-python.yaml"])[0],
        "results.json",
    )


def test_multiple_configs_different_origins(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", GITHUB_TEST_GIST_URL])[0], "results.json"
    )


def test_metavariable_propagation_regex(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-regex-propagation.yaml",
            target_name="metavariable_propagation/metavariable-regex-propagation.py",
        )[0],
        "results.json",
    )


def test_metavariable_propagation_comparison(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-comparison-propagation.yaml",
            target_name="metavariable_propagation/metavariable-comparison-propagation.py",
        )[0],
        "results.json",
    )


def test_taint_mode(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint.yaml",
            target_name="taint/taint.py",
        )[0],
        "results.json",
    )


def test_deduplication_same_message(run_semgrep_in_tmp, snapshot):
    """
    With same message, should deduplicate and only have one finding
    """
    output, _ = run_semgrep_in_tmp(
        "rules/deduplication/duplication-same-message.yaml",
        target_name="deduplication/deduplication.py",
    )
    snapshot.assert_match(output, "results.json")
    json_output = json.loads(output)
    assert len(json_output["results"]) == 1


def test_deduplication_different_message(run_semgrep_in_tmp, snapshot):
    output, _ = run_semgrep_in_tmp(
        "rules/deduplication/duplication-different-message.yaml",
        target_name="deduplication/deduplication.py",
    )
    snapshot.assert_match(output, "results.json")
    json_output = json.loads(output)
    assert len(json_output["results"]) == 2


def test_pattern_regex_empty_file(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-regex-empty-file.yaml",
            target_name="empty/totally_empty_file",
        )[0],
        "results.json",
    )
