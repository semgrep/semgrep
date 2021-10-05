import collections
import json
from typing import Callable
from typing import Dict
from typing import Mapping
from xml.etree import cElementTree

import pytest
from tests.conftest import _clean_output_json

from semgrep import __VERSION__
from semgrep.constants import OutputFormat


# https://stackoverflow.com/a/10077069
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
    "--json": _clean_output_json,
}


# junit-xml is tested in a test_junit_xml_output due to ambiguous XML attribute ordering
@pytest.mark.parametrize("format", ["--json", "--sarif", "--emacs", "--vim"])
def test_output_format(run_semgrep_in_tmp, snapshot, format):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=[format],
        output_format=OutputFormat.TEXT,  # Not the real output format; just disables JSON parsing
    )
    clean = CLEANERS.get(format, lambda s: s)(stdout)
    snapshot.assert_match(clean, "results.out")


def test_omit_inventory(run_semgrep_in_tmp, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/inventory/invent.yaml", target_name="inventory/invent.py"
    )
    snapshot.assert_match(stdout, "results.out")


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
