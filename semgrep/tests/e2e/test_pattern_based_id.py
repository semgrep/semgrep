import json
import tempfile
from pathlib import Path

import pytest

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
def test_duplicate_matches_indexing(run_semgrep_in_tmp, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/pattern_based_id/duplicates.yaml",
        target_name="pattern-based-id/duplicates",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(results, "results.json")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target,change",
    [
        # ("rules/pattern_based_id/","",True)
        ("rules/pattern_based_id/formatting.yaml", "formatting.c", False),
        ("rules/pattern_based_id/formatting.yaml", "ellipse.c", False),
        ("rules/taint.yaml", "taint.py", False),
        ("rules/pattern_based_id/operator.yaml", "operator.c", True),
        ("rules/pattern_based_id/formatting.yaml", "meta-change.c", True),
        ("rules/pattern_based_id/join.yaml", "join.py", True),
    ],
)
def test_id_change(run_semgrep_in_tmp, snapshot, rule, target, change):
    # Since the id is based on the file name, we want the exact same file
    # everytime, for both the before and after files
    with tempfile.NamedTemporaryFile(dir=Path("targets")) as tf:
        with open(Path("targets/pattern-based-id/before") / target) as fin:
            tf.write(fin.read().encode("utf-8"))
        tf.flush()  # Make sure file has been copied.
        tf.seek(
            0
        )  # Seek to beginning since Semgrep will be reading from it. Just in case.
        print(rule, tf.name)
        before_results, _errors = run_semgrep_in_tmp(
            rule,
            target_name=tf.name,
            output_format=OutputFormat.JSON,
        )
        print(before_results, _errors)
        before_id = json.loads(before_results)["results"][0]["extra"]["fingerprint"]
        tf.seek(0)  # Seek to beginning again so we can read after file
        tf.truncate()  # delete before content
        tf.flush()  # Make sure file has been copied.
        with open(Path("targets/pattern-based-id/after") / target) as fin:
            tf.write(fin.read().encode("utf-8"))
        tf.flush()  # Make sure file has been copied.
        tf.seek(
            0
        )  # Seek to beginning since Semgrep will be reading from it. Just in case.
        print(tf.read())
        after_results, _errors = run_semgrep_in_tmp(
            rule,
            target_name=tf.name,
            output_format=OutputFormat.JSON,
        )
        print(after_results, _errors)
        after_id = json.loads(after_results)["results"][0]["extra"]["fingerprint"]

        print(before_id, after_id)
        assert (after_id != before_id) == change


# def test_taint_change():
#    assert True

# def test_metavariable_focus_change():
#    assert True
