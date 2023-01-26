from pathlib import Path

import pytest

from ..conftest import TESTS_PATH


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/dependency_aware/awscli_vuln.yaml",
            "dependency_aware/awscli",
        ),
        (
            "rules/dependency_aware/lodash-4.17.19.yaml",
            "dependency_aware/lodash",
        ),
        (
            "rules/dependency_aware/no-pattern.yaml",
            "dependency_aware/yarn",
        ),
        (
            "rules/dependency_aware/yarn-sass.yaml",
            "dependency_aware/yarn",
        ),
        ("rules/dependency_aware/go-sca.yaml", "dependency_aware/go"),
        ("rules/dependency_aware/ruby-sca.yaml", "dependency_aware/ruby"),
        ("rules/dependency_aware/log4shell.yaml", "dependency_aware/log4j"),
        ("rules/dependency_aware/rust-sca.yaml", "dependency_aware/rust"),
        ("rules/dependency_aware/ansi-html.yaml", "dependency_aware/ansi"),
        ("rules/dependency_aware/js-sca.yaml", "dependency_aware/js"),
        ("rules/dependency_aware/generic-sca.yaml", "dependency_aware/generic"),
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry",
        ),
        (
            "rules/dependency_aware/monorepo.yaml",
            "dependency_aware/monorepo/",
        ),
        (
            "rules/dependency_aware/nested_package_lock.yaml",
            "dependency_aware/nested_package_lock/",
        ),
        ("rules/dependency_aware/js-yarn2-sca.yaml", "dependency_aware/yarn2"),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements",
        ),
        (
            "rules/dependency_aware/transitive_and_direct.yaml",
            "dependency_aware/transitive_and_direct/transitive_not_reachable_if_direct",
        ),
        (
            "rules/dependency_aware/transitive_and_direct.yaml",
            "dependency_aware/transitive_and_direct/direct_reachable_transitive_unreachable",
        ),
    ],
)
def test_dependency_aware_rules(run_semgrep_on_copied_files, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_on_copied_files(rule, target_name=target).as_snapshot(),
        "results.txt",
    )


# Quite awkward. To test that we can handle a target whose toplevel parent
# contains no lockfiles for the language in our rule, we need to _not_ pass in
# a target that begins with "targets", as that dir contains every kind of lockfile
# So we add the keyword arg to run_semgrep and manually do some cd-ing
@pytest.mark.kinda_slow
def test_no_lockfiles(run_semgrep, monkeypatch, tmp_path, snapshot):
    (tmp_path / "targets").symlink_to(Path(TESTS_PATH / "e2e" / "targets").resolve())
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "e2e" / "rules").resolve())
    monkeypatch.chdir(tmp_path / "targets" / "basic")

    snapshot.assert_match(
        run_semgrep(
            "../../rules/dependency_aware/js-sca.yaml",
            target_name="stupid.js",
            assume_targets_dir=False,
        ).as_snapshot(),
        "results.txt",
    )
