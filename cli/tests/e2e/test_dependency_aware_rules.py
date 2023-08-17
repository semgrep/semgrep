import logging
from pathlib import Path

import pytest
from tests.fixtures import RunSemgrep

from ..conftest import TESTS_PATH
from semdep.package_restrictions import is_in_range
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven

pytestmark = pytest.mark.kinda_slow


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/dependency_aware/awscli_vuln.yaml",
            "dependency_aware/awscli",
        ),
        (
            "rules/dependency_aware/awscli_vuln.yaml",
            "dependency_aware/awscli-with-manifest",
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
            "rules/dependency_aware/no-pattern.yaml",
            "dependency_aware/yarn-v1-without-version-constraint",
        ),
        (
            "rules/dependency_aware/yarn-sass.yaml",
            "dependency_aware/yarn",
        ),
        ("rules/dependency_aware/go-sca.yaml", "dependency_aware/go"),
        ("rules/dependency_aware/ruby-sca.yaml", "dependency_aware/ruby"),
        (
            "rules/dependency_aware/ruby-sca.yaml",
            "dependency_aware/ruby-with-multiple-remotes",
        ),
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
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle_trailing_newline",
        ),
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle_empty=",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry_with_arbitrary_starting_comment",
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
        ("rules/dependency_aware/js-pnpm-sca.yaml", "dependency_aware/pnpm"),
        ("rules/dependency_aware/js-pnpm-sca.yaml", "dependency_aware/pnpm-workspaces"),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements3",
        ),
        (
            "rules/dependency_aware/transitive_and_direct.yaml",
            "dependency_aware/transitive_and_direct/transitive_not_reachable_if_direct",
        ),
        (
            "rules/dependency_aware/transitive_and_direct.yaml",
            "dependency_aware/transitive_and_direct/direct_reachable_transitive_unreachable",
        ),
        (
            "rules/dependency_aware/no-pattern.yaml",
            "dependency_aware/yarn_multi_hash",
        ),
        (
            "rules/dependency_aware/yarn-sass.yaml",
            "dependency_aware/yarn_at_in_version",
        ),
        (
            "rules/dependency_aware/maven-guice.yaml",
            "dependency_aware/maven_dep_tree_extra_field",
        ),
        (
            "rules/dependency_aware/maven-guice.yaml",
            "dependency_aware/maven_dep_tree_joined",
        ),
        (
            "rules/dependency_aware/maven-guice.yaml",
            "dependency_aware/maven_dep_tree_optional",
        ),
        (
            "rules/dependency_aware/maven-guice.yaml",
            "dependency_aware/maven_dep_tree_release_version",
        ),
        (
            "rules/dependency_aware/js-sca.yaml",
            "dependency_aware/package-lock_resolved_false",
        ),
        (
            "rules/dependency_aware/js-sca.yaml",
            "dependency_aware/deeply_nested_package_lock",
        ),
        (
            "rules/dependency_aware/js-yarn2-sca.yaml",
            "dependency_aware/package-lock-v3",
        ),
        (
            "rules/dependency_aware/php-sca.yaml",
            "dependency_aware/php",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry_quoted_key",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry_comments",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry_empty_table",
        ),
        # This test should produce a parse error in the manifest file, but it should *still* produce findings, because the lockfile can be parsed
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/manifest_parse_error",
        ),
        (
            "rules/dependency_aware/nuget-sca-simple.yaml",
            "dependency_aware/nuget",
        ),
        # This test intentionally runs poetry rules against C# to check that scan runs correctly and does not produce findings
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/nuget",
        ),
    ],
)
def test_dependency_aware_rules(
    run_semgrep_on_copied_files: RunSemgrep, snapshot, rule, target
):
    snapshot.assert_match(
        run_semgrep_on_copied_files(rule, target_name=target).as_snapshot(),
        "results.txt",
    )


@pytest.mark.parametrize(
    "version,specifier,outcome",
    [
        ("1.2-beta-2", "> 1.0, < 1.2", True),
        ("1.2-beta-2", "> 1.2-alpha-6, < 1.2-beta-3", True),
        ("1.0.10.1", "< 1.0.10.2", True),
        ("1.3.4-SNAPSHOT", "< 1.3.4", True),
        ("1.0-SNAPSHOT", "> 1.0-alpha", True),
        ("2.17.2", "< 2.3.1", False),
        ("2.0", "< 1.0", False),
        ("2.0.0", "< 10.0.0", True),
        ("0.2.0", "< 0.10.0", True),
        ("0.0.2", "< 0.0.10", True),
        ("2.14.0", "< 2.9.10.3", False),
        ("2.14.0-beta", "< 2.9.10.3", False),
        ("1.1.1.1-SNAPSHOT", "< 1.1.1.1", True),
    ],
)
@pytest.mark.no_semgrep_cli
def test_maven_version_comparison(version, specifier, outcome):
    assert is_in_range(Ecosystem(Maven()), specifier, version) == outcome


@pytest.mark.parametrize(
    "target",
    [
        "targets/dependency_aware/osv_parsing/requirements/empty/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/only-comments/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/file-format-example/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/with-added-support/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/multiple-packages-mixed/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/multiple-packages-constrained/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/one-package-unconstrained/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/non-normalized-names/requirements.txt",
        "targets/dependency_aware/osv_parsing/requirements/one-package-constrained/requirements.txt",
        "targets/dependency_aware/osv_parsing/yarn/metadata-only.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/files.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/versions-with-build-strings.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/scoped-packages.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/one-package.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/commits.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/two-packages.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/multiple-versions.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/empty.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/files.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/scoped-packages.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/versions-with-build-strings.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/one-package.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/two-packages.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/commits.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/multiple-versions.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/empty.v2/yarn.lock",
        "targets/dependency_aware/osv_parsing/yarn/multiple-constraints.v1/yarn.lock",
        "targets/dependency_aware/osv_parsing/package-lock/files.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/nested-dependencies-dup.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/scoped-packages.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/one-package-dev.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/one-package.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/commits.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/two-packages.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/nested-dependencies.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/empty.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/files.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/one-package-dev.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/scoped-packages.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/nested-dependencies-dup.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/one-package.v1/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/two-packages.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/commits.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/nested-dependencies.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/package-lock/empty.v2/package-lock.json",
        "targets/dependency_aware/osv_parsing/pipfile/empty/Pipfile.lock",
        "targets/dependency_aware/osv_parsing/pipfile/one-package/Pipfile.lock",
        "targets/dependency_aware/osv_parsing/pipfile/no-version/Pipfile.lock",
        "targets/dependency_aware/osv_parsing/pipfile/one-package-dev/Pipfile.lock",
        "targets/dependency_aware/osv_parsing/pipfile/two-packages/Pipfile.lock",
        "targets/dependency_aware/osv_parsing/pipfile/two-packages-alt/Pipfile.lock",
        "targets/dependency_aware/osv_parsing/poetry/source-legacy/poetry.lock",
        "targets/dependency_aware/osv_parsing/poetry/empty/poetry.lock",
        "targets/dependency_aware/osv_parsing/poetry/one-package/poetry.lock",
        "targets/dependency_aware/osv_parsing/poetry/one-package-with-metadata/poetry.lock",
        "targets/dependency_aware/osv_parsing/poetry/two-packages/poetry.lock",
        "targets/dependency_aware/osv_parsing/poetry/source-git/poetry.lock",
        "targets/dependency_aware/osv_parsing/pnpm/commits/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/empty/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/one-package/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/peer-dependencies-advanced/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/multiple-versions/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/no-packages/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/one-package-dev/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/scoped-packages-v6-lockfile/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/peer-dependencies/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/one-package-v6-lockfile/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/scoped-packages/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/multiple-packages/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/tarball/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/files/pnpm-lock.yaml",
        "targets/dependency_aware/osv_parsing/pnpm/exotic/pnpm-lock.yaml",
    ],
)
# These tests are taken from https://github.com/google/osv-scanner/tree/main/pkg/lockfile/fixtures
# With some minor edits, namely removing the "this isn't even a lockfile" tests
# And removing some human written comments that would never appear in a real lockfile from some tests
@pytest.mark.no_semgrep_cli
def test_osv_parsing(parse_lockfile_path_in_tmp, caplog, target):
    caplog.set_level(logging.ERROR)
    _, error = parse_lockfile_path_in_tmp(Path(target))
    assert len(caplog.records) == 0
    assert len(error) == 0


# Quite awkward. To test that we can handle a target whose toplevel parent
# contains no lockfiles for the language in our rule, we need to _not_ pass in
# a target that begins with "targets", as that dir contains every kind of lockfile
# So we add the keyword arg to run_semgrep and manually do some cd-ing
def test_no_lockfiles(run_semgrep: RunSemgrep, monkeypatch, tmp_path, snapshot):
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
