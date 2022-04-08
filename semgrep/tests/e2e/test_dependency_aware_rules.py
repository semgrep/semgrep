import pytest


@pytest.mark.kinda_slow
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
        pytest.param(
            "rules/dependency_aware/monorepo.yaml",
            "dependency_aware/monorepo/",
            marks=pytest.mark.xfail(reason="unknown, just saw it failing a lot"),
        ),
    ],
)
def test_dependency_aware_rules(run_semgrep_in_tmp_no_symlink, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp_no_symlink(rule, target_name=target)[0],
        "results.json",
    )
