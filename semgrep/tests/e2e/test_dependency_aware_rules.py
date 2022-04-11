from pathlib import Path

import pytest

from ..conftest import _run_semgrep
from ..conftest import TESTS_PATH


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
        ("rules/dependency_aware/generic-sca.yaml", "dependency_aware/generic.txt"),
        ("rules/dependency_aware/monorepo.yaml", "dependency_aware/monorepo/"),
    ],
)
def test_dependency_aware_rules(run_semgrep_in_tmp_no_symlink, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp_no_symlink(rule, target_name=target)[0],
        "results.json",
    )


# Quite awkward. To test that we can handle a target whose toplevel parent
# contains no lockfiles for the language in our rule, we need to _not_ pass in
# a target that begins with "targets", as that dir contains every kind of lockfile
# So we add the keyword arg to _run_semgrep and manually do some cd-ing
@pytest.mark.kinda_slow
def test_no_lockfiles(monkeypatch, tmp_path, snapshot):
    (tmp_path / "targets").symlink_to(Path(TESTS_PATH / "e2e" / "targets").resolve())
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "e2e" / "rules").resolve())
    monkeypatch.chdir(tmp_path / "targets" / "basic")

    snapshot.assert_match(
        _run_semgrep(
            "../../rules/dependency_aware/js-sca.yaml",
            target_name="stupid.js",
            assume_targets_dir=False,
        )[0],
        "results.json",
    )
