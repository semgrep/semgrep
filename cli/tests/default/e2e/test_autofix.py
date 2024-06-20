import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.osemfail
@pytest.mark.kinda_slow
@pytest.mark.parametrize("dryrun", [True, False], ids=["dryrun", "not-dryrun"])
@pytest.mark.parametrize(
    "output_format", [OutputFormat.JSON, OutputFormat.TEXT], ids=["json", "text"]
)
# The rule is looked in rules/autofix/ and the target in autofix/ but
# we use shorter names below so the snapshot dirs use less characters
# which can be an issue on Windows.
@pytest.mark.parametrize(
    "rule,target",
    [
        ("autofix.yaml", "autofix.py"),
        ("overlapping-collision.yaml", "collision.py"),
        ("python-assert-statement.yaml", "python-assert-statement.py"),
        ("python-ranges.yaml", "python-ranges.py"),
        ("replace-field-yaml.yaml", "replace-field-yaml.yaml"),
        ("three-autofixes.yaml", "three-autofixes.py"),
        ("java-string-wrap.yaml", "java-string-wrap.java"),
        ("exact-collision.yaml", "collision.py"),
        ("order.yaml", "order.py"),
        ("redundant.yaml", "redundant.py"),
        ("ocaml_paren_expr.yaml", "ocaml_paren_expr.ml"),
        ("python-delete-import.yaml", "python-delete-import.py"),
        ("two-autofixes.yaml", "two-autofixes.txt"),
        ("csv-writer.yaml", "csv-writer.py"),
        ("defaulthttpclient.yaml", "defaulthttpclient.java"),
        ("flask-use-jsonify.yaml", "flask-use-jsonify.py"),
        ("requests-use-timeout.yaml", "requests-use-timeout.py"),
        ("django-none-password-default.yaml", "django-none-password-default.py"),
        ("imported-entity.yaml", "imported-entity.py"),
        ("add-metadata-hcl.yaml", "add-metadata-hcl.hcl"),
        ("delete-partial-line.yaml", "delete-partial-line.py"),
        ("utf-8.yaml", "utf-8.py"),
    ],
)
def test(
    run_semgrep_on_copied_files: RunSemgrep,
    tmp_path,
    snapshot,
    rule,
    target,
    dryrun,
    output_format,
):
    rule = "rules/autofix/" + rule
    target = "autofix/" + target
    # Use run_semgrep_on_copied_files to prevent alteration of the
    # source-controlled test directory
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
