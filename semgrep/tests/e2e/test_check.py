import json
from pathlib import Path
from subprocess import CalledProcessError

import pytest
from xmldiff import main

from semgrep import __VERSION__

GITHUB_TEST_GIST_URL = (
    "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"
)


def test_basic_rule__local(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/eqeq.yaml"), "results.json")


def test_basic_rule__relative(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/../rules/eqeq.yaml"), "results.json",
    )


def test_noextension_filtering(run_semgrep_in_tmp, snapshot):
    """
        Check that semgrep does not filter out files without extensions when
        said file is explicitly passed
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml", target_name="basic/stupid_no_extension"
        ),
        "results.json",
    )


def test_basic_rule__absolute(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(Path.cwd() / "rules" / "eqeq.yaml"), "results.json",
    )


def test_terminal_output(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq.yaml", output_format="normal"), "output.txt"
    )


def test_multiline(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq.yaml", target_name="multiline"), "results.json",
    )


def test_junit_xml_output(run_semgrep_in_tmp, snapshot):
    actual_output = run_semgrep_in_tmp("rules/eqeq.yaml", output_format="junit-xml")

    f = open(str(snapshot.snapshot_dir) + "/results.xml", "r")
    expected_output = f.read()
    f.close()

    assert len(main.diff_texts(expected_output, actual_output)) == 0


def test_sarif_output(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq.yaml", output_format="sarif")
    )

    # rules are logically a set so the JSON list's order doesn't matter
    # we make the order deterministic here so that snapshots match across runs
    # the proper solution will be https://github.com/joseph-roitman/pytest-snapshot/issues/14
    sarif_output["runs"][0]["tool"]["driver"]["rules"] = sorted(
        sarif_output["runs"][0]["tool"]["driver"]["rules"], key=lambda rule: rule["id"]
    )

    # Semgrep version is included in sarif output. Verify this independently so
    # snapshot does not need to be updated on version bump
    assert sarif_output["runs"][0]["tool"]["driver"]["semanticVersion"] == __VERSION__
    sarif_output["runs"][0]["tool"]["driver"]["semanticVersion"] = "placeholder"

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )


def test_url_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(GITHUB_TEST_GIST_URL), "results.json",
    )


def test_registry_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("r2c"), "results.json",
    )


def test_hidden_rule__explicit(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/hidden/.hidden"), "results.json")


def test_hidden_rule__implicit(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/hidden")
    assert excinfo.value.returncode == 7
    snapshot.assert_match(excinfo.value.stdout, "error.json")

    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/hidden", output_format="normal")
    assert excinfo.value.returncode == 7
    snapshot.assert_match(excinfo.value.stderr, "error.txt")


def test_default_rule__file(run_semgrep_in_tmp, snapshot):
    Path(".semgrep.yml").symlink_to(Path("rules/eqeq.yaml").resolve())
    snapshot.assert_match(run_semgrep_in_tmp(), "results.json")


def test_default_rule__folder(run_semgrep_in_tmp, snapshot):
    Path(".semgrep").mkdir()
    Path(".semgrep/.semgrep.yml").symlink_to(Path("rules/eqeq.yaml").resolve())

    snapshot.assert_match(run_semgrep_in_tmp(), "results.json")


def test_regex_rule__top(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/regex-top.yaml"), "results.json")


def test_regex_rule__child(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/regex-child.yaml"), "results.json")


def test_regex_rule__invalid_expression(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/regex-invalid.yaml")
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_nested_patterns_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-patterns.yaml"), "results.json"
    )


def test_nosem_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/nosem.yaml"), "results.json")


def test_nosem_rule__invalid_id(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/nosem.yaml", target_name="nosem_invalid_id")
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_metavariable_regex_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex.yaml"), "results.json"
    )


def test_metavariable_regex_multi_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-rule.yaml"), "results.json"
    )


def test_metavariable_multi_regex_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-regex.yaml"), "results.json"
    )


def test_regex_with_any_language_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language.yaml", target_name="basic/regex-any-language.html"
        ),
        "results.json",
    )


def test_regex_with_any_language_multiple_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-multiple.yaml",
            target_name="basic/regex-any-language.html",
        ),
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


def test_timeout(run_semgrep_in_tmp, snapshot):
    # Check that semgrep-core timeouts are properly handled

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            options=["--timeout", "1"],
            target_name="equivalence",
            strict=False,
        ),
        "results.json",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            output_format="normal",
            options=["--timeout", "1"],
            target_name="equivalence",
            strict=False,
            stderr=True,
        ),
        "error.txt",
    )


def test_max_memory(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            options=["--verbose", "--max-memory", "1"],
            target_name="equivalence",
            strict=False,
        ),
        "results.json",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            output_format="normal",
            options=["--verbose", "--max-memory", "1"],
            target_name="equivalence",
            strict=False,
            stderr=True,
        ),
        "error.txt",
    )


def test_timeout_threshold(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
        ),
        "results.json",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format="normal",
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
            stderr=True,
        ),
        "error.txt",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format="normal",
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "2"],
            target_name="equivalence",
            strict=False,
            stderr=True,
        ),
        "error_2.txt",
    )


def test_metavariable_comparison_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison.yaml"), "results.json"
    )


def test_metavariable_comparison_rule_base(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-base.yaml"), "results.json"
    )


def test_metavariable_comparison_rule_strip(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-strip.yaml"), "results.json"
    )


def test_metavariable_comparison_rule_bad_content(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-bad-content.yaml"),
        "results.json",
    )


def test_multiple_configs_file(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", "rules/eqeq-python.yaml"]),
        "results.json",
    )


def test_multiple_configs_different_origins(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", GITHUB_TEST_GIST_URL]), "results.json"
    )
