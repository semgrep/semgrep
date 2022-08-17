import pytest


@pytest.mark.kinda_slow
@pytest.mark.parametrize("dryrun", [True, False], ids=["dryrun", "not-dryrun"])
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/autofix/autofix.yaml", "autofix/autofix.py"),
        ("rules/autofix/csv-writer.yaml", "autofix/csv-writer.py"),
        ("rules/autofix/defaulthttpclient.yaml", "autofix/defaulthttpclient.java"),
        ("rules/autofix/flask-use-jsonify.yaml", "autofix/flask-use-jsonify.py"),
        ("rules/autofix/requests-use-timeout.yaml", "autofix/requests-use-timeout.py"),
        (
            "rules/autofix/django-none-password-default.yaml",
            "autofix/django-none-password-default.py",
        ),
        (
            "rules/autofix/terraform-ec2-instance-metadata-options.yaml",
            "autofix/terraform-ec2-instance-metadata-options.hcl",
        ),
        (
            "rules/autofix/python-assert-statement.yaml",
            "autofix/python-assert-statement.py",
        ),
        ("rules/autofix/java-string-wrap.yaml", "autofix/java-string-wrap.java"),
        ("rules/autofix/two-autofixes.yaml", "autofix/two-autofixes.txt"),
        ("rules/autofix/three-autofixes.yaml", "autofix/three-autofixes.py"),
        ("rules/autofix/ocaml_paren_expr.yaml", "autofix/ocaml_paren_expr.ml"),
        ("rules/autofix/python-ranges.yaml", "autofix/python-ranges.py"),
    ],
)
@pytest.mark.kinda_slow
def test_autofix(run_semgrep_on_copied_files, tmp_path, snapshot, rule, target, dryrun):

    # Use run_semgrep_on_copied_files to prevent alteration of the source-controlled test directory
    result = run_semgrep_on_copied_files(
        rule,
        target_name=target,
        options=["--autofix", *(["--dryrun"] if dryrun else [])],
    )
    snapshot.assert_match(
        result.stdout,
        "results.json",
    )

    # Now make sure the files are actually updated
    with open(tmp_path / "targets" / target) as fd:
        result = fd.read()
    snapshot.assert_match(
        result,
        (f"{target}-dryrun" if dryrun else f"{target}-fixed"),
    )
