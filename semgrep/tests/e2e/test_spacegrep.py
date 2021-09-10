import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/spacegrep/terraform.yaml", "spacegrep/terraform.tf"),
        ("rules/spacegrep/html.yaml", "spacegrep/html.mustache"),
        ("rules/spacegrep/markdown.yaml", "spacegrep/markdown.md"),
        ("rules/spacegrep/httpresponse.yaml", "spacegrep/httpresponse.txt"),
        ("rules/spacegrep/dockerfile.yaml", "spacegrep/root.Dockerfile"),
        ("rules/spacegrep/multi-lines.yaml", "spacegrep/multi-lines.java"),
    ],
)
def test_spacegrep(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0],
        "results.json",
    )


@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/spacegrep/nosem-html.yaml", "spacegrep/nosem.html"),
    ],
)
def test_spacegrep_nosem(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target, options=["--no-rewrite-rule-ids"])[
            0
        ],
        "results.json",
    )
