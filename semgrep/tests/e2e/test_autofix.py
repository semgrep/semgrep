import tempfile
from pathlib import Path

import pytest


def test_autofix(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/autofix/autofix.yaml", target_name="autofix/autofix.py"
        ),
        "results.json",
    )

    # Make a copy of the target file b/c autofixes are inline. We
    # don't want to modify the the actual target file, and there
    # isn't a way currently to dump the fixed file contents before
    # writing.
    # This tempfile will be deleted when the with context closes.
    with tempfile.NamedTemporaryFile(dir=Path("targets")) as tf:
        with open(Path("targets") / "autofix/autofix.py", "r") as fin:
            tf.write(fin.read().encode("utf-8"))
        tf.flush()  # Make sure file has been copied.
        tf.seek(
            0
        )  # Seek to beginning since Semgrep will be reading from it. Just in case.
        run_semgrep_in_tmp(
            "rules/autofix/autofix.yaml", target_name=tf.name, options=["--autofix"]
        )
        tf.seek(0)  # Seek to beginning again so we can read and compare to snapshot.
        snapshot.assert_match(tf.read().decode("utf-8"), "autofix-fixed")


@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/autofix/csv-writer.yaml", "autofix/csv-writer.py"),
        ("rules/autofix/defaulthttpclient.yaml", "autofix/defaulthttpclient.java"),
        ("rules/autofix/flask-use-jsonify.yaml", "autofix/flask-use-jsonify.py"),
        ("rules/autofix/requests-use-timeout.yaml", "autofix/requests-use-timeout.py"),
        (
            "rules/autofix/django-none-password-default.yaml",
            "autofix/django-none-password-default.py",
        ),
    ],
)
def test_regex_autofix(run_semgrep_in_tmp, snapshot, rule, target):
    # Yes, this is fugly. I apologize. T_T
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target), "results.json",
    )
    # Make a copy of the target file b/c autofixes are inline. We
    # don't want to modify the the actual target file, and there
    # isn't a way currently to dump the fixed file contents before
    # writing.
    # This tempfile will be deleted when the with context closes.
    with tempfile.NamedTemporaryFile(dir=Path("targets")) as tf:
        with open(Path("targets") / target, "r") as fin:
            tf.write(fin.read().encode("utf-8"))
        tf.flush()  # Make sure file has been copied.
        tf.seek(
            0
        )  # Seek to beginning since Semgrep will be reading from it. Just in case.
        run_semgrep_in_tmp(rule, target_name=tf.name, options=["--autofix"])
        tf.seek(0)  # Seek to beginning again so we can read and compare to snapshot.
        snapshot.assert_match(tf.read().decode("utf-8"), f"{target}-fixed")
