import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.osemfail
@pytest.mark.kinda_slow
@pytest.mark.parametrize("dryrun", [True, False], ids=["dryrun", "not-dryrun"])
@pytest.mark.parametrize(
    "output_format", [OutputFormat.JSON, OutputFormat.TEXT], ids=["json", "text"]
)
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/autofix/autofix.yaml", "autofix/autofix.py"),
        ("rules/autofix/overlapping-collision.yaml", "autofix/collision.py"),
        (
            "rules/autofix/python-assert-statement.yaml",
            "autofix/python-assert-statement.py",
        ),
        ("rules/autofix/python-ranges.yaml", "autofix/python-ranges.py"),
        (
            "rules/autofix/yaml-excessive-mapping-capture.yaml",
            "autofix/yaml-excessive-mapping-capture.yaml",
        ),
        ("rules/autofix/three-autofixes.yaml", "autofix/three-autofixes.py"),
        ("rules/autofix/java-string-wrap.yaml", "autofix/java-string-wrap.java"),
        ("rules/autofix/exact-collision.yaml", "autofix/collision.py"),
        ("rules/autofix/order.yaml", "autofix/order.py"),
        ("rules/autofix/redundant.yaml", "autofix/redundant.py"),
        ("rules/autofix/ocaml_paren_expr.yaml", "autofix/ocaml_paren_expr.ml"),
        ("rules/autofix/python-delete-import.yaml", "autofix/python-delete-import.py"),
        ("rules/autofix/two-autofixes.yaml", "autofix/two-autofixes.txt"),
        ("rules/autofix/csv-writer.yaml", "autofix/csv-writer.py"),
        ("rules/autofix/defaulthttpclient.yaml", "autofix/defaulthttpclient.java"),
        ("rules/autofix/flask-use-jsonify.yaml", "autofix/flask-use-jsonify.py"),
        ("rules/autofix/requests-use-timeout.yaml", "autofix/requests-use-timeout.py"),
        (
            "rules/autofix/django-none-password-default.yaml",
            "autofix/django-none-password-default.py",
        ),
        ("rules/autofix/imported-entity.yaml", "autofix/imported-entity.py"),
        (
            "rules/autofix/terraform-ec2-instance-metadata-options.yaml",
            "autofix/terraform-ec2-instance-metadata-options.hcl",
        ),
        ("rules/autofix/delete-partial-line.yaml", "autofix/delete-partial-line.py"),
    ],
)
def test_autofix(
    run_semgrep_on_copied_files: RunSemgrep,
    tmp_path,
    snapshot,
    rule,
    target,
    dryrun,
    output_format,
):
    # Use run_semgrep_on_copied_files to prevent alteration of the source-controlled test directory
    semgrep_result = run_semgrep_on_copied_files(
        rule,
        target_name=target,
        options=["--autofix", *(["--dryrun"] if dryrun else [])],
        output_format=output_format,
    )
    if output_format == OutputFormat.JSON:
        results_file = "results.json"
    else:
        results_file = "results.txt"
    snapshot.assert_match(
        semgrep_result.stdout,
        results_file,
    )

    snapshot.assert_match(
        semgrep_result.stderr,
        "stderr.txt",
    )

    # Now make sure the files are actually updated
    with open(tmp_path / "targets" / target) as fd:
        result = fd.read()
    snapshot.assert_match(
        result,
        (f"{target}-dryrun" if dryrun else f"{target}-fixed"),
    )
