import collections
import json
import re
from pathlib import Path
from typing import Callable
from typing import Dict
from typing import Mapping
from xml.etree import cElementTree

import pytest

from semgrep import __VERSION__
from semgrep.constants import OutputFormat
from tests.conftest import _clean_output_json
from tests.conftest import TESTS_PATH


# https://stackoverflow.com/a/10077069
@pytest.mark.kinda_slow
def _etree_to_dict(t):
    """
    A simple and sufficient XML -> dict conversion function. This function is
    used to perform basic XML test data comparisons.
    """
    d: Dict[str, Dict] = {t.tag: {}}
    children = list(t)
    if children:
        dd = collections.defaultdict(list)
        for dc in map(_etree_to_dict, children):
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


@pytest.mark.kinda_slow
def _clean_sarif_output(output):
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


CLEANERS: Mapping[str, Callable[[str], str]] = {
    "--sarif": lambda s: json.dumps(_clean_sarif_output(json.loads(s))),
    "--gitlab-sast": _clean_output_json,
    "--gitlab-secrets": _clean_output_json,
    "--json": _clean_output_json,
}


@pytest.mark.kinda_slow
def test_output_highlighting(run_semgrep_in_tmp, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        target_name="cli_test/basic/",
        output_format=OutputFormat.TEXT,
        strict=False,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_output_highlighting__no_color(run_semgrep_in_tmp, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        target_name="cli_test/basic/",
        output_format=OutputFormat.TEXT,
        strict=False,
        env={"NO_COLOR": "1"},
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_output_highlighting__force_color_and_no_color(run_semgrep_in_tmp, snapshot):
    """
    NO_COLOR would normally disable color: https://no-color.org/

    But a tool specific flag should override a global flag.
    So when both are set, we should have color.
    """
    results, _errors = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        target_name="cli_test/basic/",
        output_format=OutputFormat.TEXT,
        strict=False,
        force_color=True,
        env={"NO_COLOR": "1"},
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


# junit-xml is tested in a test_junit_xml_output due to ambiguous XML attribute ordering
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "format",
    ["--json", "--gitlab-sast", "--gitlab-secrets", "--sarif", "--emacs", "--vim"],
)
def test_output_format(run_semgrep_in_tmp, snapshot, format):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=[format],
        output_format=OutputFormat.TEXT,  # Not the real output format; just disables JSON parsing
    )
    clean = CLEANERS.get(format, lambda s: s)(stdout)
    snapshot.assert_match(clean, "results.out")


@pytest.mark.kinda_slow
def test_omit_inventory(run_semgrep_in_tmp, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/inventory/invent.yaml", target_name="inventory/invent.py"
    )
    snapshot.assert_match(stdout, "results.out")


@pytest.mark.kinda_slow
def test_junit_xml_output(run_semgrep_in_tmp, snapshot):
    output, _ = run_semgrep_in_tmp(
        "rules/eqeq.yaml", output_format=OutputFormat.JUNIT_XML
    )
    result = _etree_to_dict(cElementTree.XML(output))

    filename = snapshot.snapshot_dir / "results.xml"
    expected = _etree_to_dict(cElementTree.XML(filename.read_text()))

    assert expected == result


# If there are nosemgrep comments to ignore findings, SARIF output should include them
# labeled as suppressed.
@pytest.mark.kinda_slow
def test_sarif_output_include_nosemgrep(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp(
            "rules/regex-nosemgrep.yaml",
            target_name="basic/regex-nosemgrep.txt",
            output_format=OutputFormat.SARIF,
        )[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )


@pytest.mark.kinda_slow
def test_sarif_output_with_source(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq-source.yml", output_format=OutputFormat.SARIF)[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )

    # Assert that each sarif rule object has a helpURI
    for rule in sarif_output["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("helpUri", None) is not None


@pytest.mark.kinda_slow
def test_sarif_output_with_source_edit(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq-meta.yaml", output_format=OutputFormat.SARIF)[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )

    # Assert that each sarif rule object has a helpURI
    for rule in sarif_output["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("help", None) is not None


@pytest.mark.kinda_slow
def test_sarif_output_with_nosemgrep_and_error(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="nosemgrep/eqeq-nosemgrep.py",
            output_format=OutputFormat.SARIF,
            options=["--error"],
        )[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )


IGNORE_LOG_REPORT_FIRST_LINE = "Some files were skipped."
IGNORE_LOG_REPORT_LAST_LINE = (
    "  For a full list of skipped files, run semgrep with the --verbose flag."
)


@pytest.mark.kinda_slow
def test_semgrepignore_ignore_log_report(run_semgrep_in_tmp, tmp_path, snapshot):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TESTS_PATH / "e2e" / "targets" / "ignores" / ".semgrepignore").resolve()
    )

    _, stderr = run_semgrep_in_tmp(
        "rules/eqeq-basic.yaml",
        # This set of options is carefully crafted
        # to trigger one entry for most ignore reasons.
        # Note that the print order is non-deterministic,
        # so you must take care not to have two skips in a category.
        options=[
            "--include=ignore.*",
            "--include=tests",
            "--include=find.*",
            "--exclude=*.min.js",
            "--max-target-bytes=100",
            "--verbose",
        ],
        output_format=OutputFormat.TEXT,
        force_color=True,
        target_name="ignores",
    )

    report = re.search(
        f"^{IGNORE_LOG_REPORT_FIRST_LINE}$.*?^{IGNORE_LOG_REPORT_LAST_LINE}$",
        stderr,
        flags=re.MULTILINE | re.DOTALL,
    )
    assert (
        report is not None
    ), "can't find ignore log report based on expected start and end lines"
    snapshot.assert_match(report.group(), "report.txt")


@pytest.mark.kinda_slow
def test_semgrepignore_ignore_log_json_report(run_semgrep_in_tmp, tmp_path, snapshot):
    (tmp_path / ".semgrepignore").symlink_to(
        Path(TESTS_PATH / "e2e" / "targets" / "ignores" / ".semgrepignore").resolve()
    )

    stdout, _ = run_semgrep_in_tmp(
        "rules/eqeq-basic.yaml",
        # This set of options is carefully crafted
        # to trigger one entry for most ignore reasons.
        # Note that the print order is non-deterministic,
        # so you must take care not to have two skips in a category.
        options=[
            "--include=ignore.*",
            "--include=tests",
            "--include=find.*",
            "--exclude=*.min.js",
            "--max-target-bytes=100",
            "--verbose",
        ],
        output_format=OutputFormat.JSON,
        target_name="ignores",
    )
    parsed_output = json.loads(stdout)
    assert "paths" in parsed_output

    snapshot.assert_match(
        json.dumps(parsed_output["paths"], indent=2, sort_keys=True), "report.json"
    )
