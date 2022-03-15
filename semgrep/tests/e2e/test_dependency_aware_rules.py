import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/dependency_aware/awscli_vuln.yaml",
            "dependency_aware/awscli_vuln.py",
        ),
        (
            "rules/dependency_aware/lodash-4.17.19.yaml",
            "dependency_aware/useslodash.js",
        ),
        ("rules/dependency_aware/go-sca.yaml", "dependency_aware/sca.go"),
        ("rules/dependency_aware/ruby-sca.yaml", "dependency_aware/sca.rb"),
        ("rules/dependency_aware/log4shell.yaml", "dependency_aware/log4shell.java"),
        ("rules/dependency_aware/rust-sca.yaml", "dependency_aware/sca.rs"),
        ("rules/dependency_aware/ansi-html.yaml", "dependency_aware/ansi.js"),
        ("rules/dependency_aware/js-sca.yaml", "dependency_aware/sca.js"),
    ],
)
def test_dependency_aware_rules(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0],
        "results.json",
    )


def test_explicit_lockfile_target(run_semgrep_in_tmp, snapshot):
    # We expect no results, because we explicitly passed a folder containing
    # a lockfile which does NOT contain the vulnerable dependency that this rule searches for
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/dependency_aware/go-sca.yaml",
            target_name="dependency_aware/sca.go",
            env={"SEMGREP_LOCKFILE_PATH": "targets/dependency_aware/subproject"},
        )[0],
        "results.json",
    )
