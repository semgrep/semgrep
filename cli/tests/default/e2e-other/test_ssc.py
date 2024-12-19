import json
import logging
from pathlib import Path
from typing import Any
from typing import Dict

import pytest
from tests.conftest import RULES_PATH
from tests.conftest import TARGETS_PATH
from tests.fixtures import RunSemgrep

from semdep.package_restrictions import is_in_range
from semdep.parsers.package_lock import parse_package_lock
from semdep.parsers.pipfile import parse_pipfile
from semdep.parsers.pnpm import parse_pnpm
from semdep.parsers.poetry import parse_poetry
from semdep.parsers.requirements import parse_requirements
from semdep.parsers.util import SemgrepParser
from semdep.parsers.yarn import parse_yarn
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
        ("rules/dependency_aware/go-sca.yaml", "dependency_aware/go_toolchain"),
        ("rules/dependency_aware/go-sca.yaml", "dependency_aware/go_multi_newline"),
        ("rules/dependency_aware/ruby-sca.yaml", "dependency_aware/ruby"),
        (
            "rules/dependency_aware/ruby-sca.yaml",
            "dependency_aware/ruby-with-multiple-remotes",
        ),
        (
            "rules/dependency_aware/ruby-sca.yaml",
            "dependency_aware/ruby-with-multiple-gem-sections",
        ),
        ("rules/dependency_aware/log4shell.yaml", "dependency_aware/log4j"),
        ("rules/dependency_aware/rust-sca.yaml", "dependency_aware/rust"),
        (
            "rules/dependency_aware/rust-sca.yaml",
            "dependency_aware/rust_short_lockfile",
        ),
        ("rules/dependency_aware/ansi-html.yaml", "dependency_aware/ansi"),
        ("rules/dependency_aware/js-sca.yaml", "dependency_aware/js"),
        ("rules/dependency_aware/generic-sca.yaml", "dependency_aware/generic"),
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle",
        ),
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle-arbitrary-comment",
        ),
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle-no-comment",
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
            "rules/dependency_aware/python-pipfile-sca.yaml",
            "dependency_aware/pipfile",
        ),
        (
            "rules/dependency_aware/python-pipfile-sca.yaml",
            "dependency_aware/pipfile_with_uppercase_package_name",
        ),
        (
            "rules/dependency_aware/python-pipfile-sca.yaml",
            "dependency_aware/pipfile_with_empty_dev-packages",
        ),
        (
            "rules/dependency_aware/python-pipfile-pyhcl-sca.yaml",
            "dependency_aware/pipfile_with_vuln_in_dev-packages",
        ),
        (
            "rules/dependency_aware/python-pipfile-sca.yaml",
            "dependency_aware/pipfile_with_one_newline_between_sections",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry",
        ),
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry_with_uppercase_package_name",
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
        ("rules/dependency_aware/js-pnpm-sca.yaml", "dependency_aware/pnpm-v6"),
        ("rules/dependency_aware/js-pnpm-sca.yaml", "dependency_aware/pnpm-v9"),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_with_uppercase_package_name",
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
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/poetry_trailing_newlines",
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
        (
            "rules/dependency_aware/nuget-sca-simple.yaml",
            "dependency_aware/nuget-large",
        ),
        # This test intentionally runs poetry rules against C# to check that scan runs correctly and does not produce findings
        (
            "rules/dependency_aware/python-poetry-sca.yaml",
            "dependency_aware/nuget",
        ),
        ("rules/dependency_aware/gradle-guava.yaml", "dependency_aware/gradle-direct"),
        (
            "rules/dependency_aware/dart-parity.yaml",
            "dependency_aware/dart",
        ),
        (
            "rules/dependency_aware/swift-sca.yaml",
            "dependency_aware/swiftpm/v1",
        ),
        (
            "rules/dependency_aware/swift-sca.yaml",
            "dependency_aware/swiftpm/v2",
        ),
        (
            "rules/dependency_aware/swift-sca.yaml",
            "dependency_aware/swiftpm/v3",
        ),
        (
            "rules/dependency_aware/swift-sca.yaml",
            "dependency_aware/swiftpm_missing_version",
        ),
    ],
)
@pytest.mark.osemfail
def test_ssc(run_semgrep_on_copied_files: RunSemgrep, snapshot, rule, target):
    result = run_semgrep_on_copied_files(rule, target_name=target)

    snapshot.assert_match(
        result.as_snapshot(),
        "results.txt",
    )


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirement",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_pip",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirement_pip",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_folder",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_pip_folder",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_folder_dep_dupes",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_folder_no_src",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_folder_similar_deps",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_multiple_lockfiles",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_multiple_lockfiles_dep_dupes",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_multiple_lockfiles_dep_dupes_no_src",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_multiple_lockfiles_no_src",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_multiple_lockfiles_similar_deps",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_nested",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_nested_no_src",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_nested_dep_dupes",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_nested_folder",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_nested_folder_dep_dupes",
        ),
        (
            "rules/dependency_aware/python-requirements-sca.yaml",
            "dependency_aware/requirements_nested_folder_no_src",
        ),
    ],
)
@pytest.mark.osemfail
def test_ssc__requirements_lockfiles(
    run_semgrep_on_copied_files: RunSemgrep, snapshot, rule: str, target: str
):
    """
    Separated out from test_ssc to avoid polluting with extra requirements lockfile tests
    """
    result = run_semgrep_on_copied_files(rule, target_name=target)

    snapshot.assert_match(
        result.as_snapshot(),
        "results.txt",
    )


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle-no-lockfile",
        ),
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle-no-lockfile-missing-gradlew",
        ),
        (
            "rules/dependency_aware/maven-log4j.yaml",
            "dependency_aware/maven-no-lockfile",
        ),
        # also include some where lockfiles are present to check that we don't break those
        (
            "rules/dependency_aware/java-gradle-sca.yaml",
            "dependency_aware/gradle",
        ),
    ],
)
@pytest.mark.requires_lockfileless_deps
def test_ssc__lockfileless(
    run_semgrep_on_copied_files: RunSemgrep, snapshot: Any, rule: str, target: str
):
    """
    Run end-to-end SSC tests with lockfileless resolution enabled.
    """
    result = run_semgrep_on_copied_files(
        rule, target_name=target, options=["--allow-local-builds"]
    )

    snapshot.assert_match(
        result.as_snapshot(),
        "results.txt",
    )


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/dependency_aware/python-poetry-case-insensitive-package.yaml",
            "dependency_aware/poetry",
        ),
        (
            "rules/dependency_aware/python-requirements-case-insensitive-package.yaml",
            "dependency_aware/requirements",
        ),
        (
            "rules/dependency_aware/python-pipfile-case-insensitive-package.yaml",
            "dependency_aware/pipfile",
        ),
    ],
)
@pytest.mark.osemfail
def test_ssc__pypi_package_name_lowercase(
    run_semgrep_on_copied_files: RunSemgrep, snapshot, rule, target
):
    """
    Pypi package names should be case insensitive
    """
    result = run_semgrep_on_copied_files(rule, target_name=target)

    snapshot.assert_match(result.as_snapshot(), "results.txt")


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
@pytest.mark.osemfail
def test_maven_version_comparison(version, specifier, outcome):
    assert is_in_range(Ecosystem(Maven()), specifier, version) == outcome


LOCKFILE_NAME_TO_PARSER: Dict[str, SemgrepParser] = {
    "requirements.txt": parse_requirements,
    "yarn.lock": parse_yarn,
    "package-lock.json": parse_package_lock,
    "Pipfile.lock": parse_pipfile,
    "poetry.lock": parse_poetry,
    "pnpm-lock.yaml": parse_pnpm,
}


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
        "targets/dependency_aware/osv_parsing/yarn/cache-compressionlevel.v2/yarn.lock",
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
        "targets/dependency_aware/pnpm-error-key/pnpm-lock.yaml",
    ],
)
# These tests are taken from https://github.com/google/osv-scanner/tree/main/pkg/lockfile/fixtures
# With some minor edits, namely removing the "this isn't even a lockfile" tests
# And removing some human written comments that would never appear in a real lockfile from some tests
# They also include random lockfiles we want to make sure we parse predictably
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
def test_parsing(caplog, target: str, snapshot, lockfile_path_in_tmp):
    # Setup
    caplog.set_level(logging.ERROR)

    target_path = Path(target)
    # Parse
    parser: SemgrepParser = LOCKFILE_NAME_TO_PARSER[target_path.name]
    dependencies, error = parser(Path(target), None)  # no manifest for any of these

    # The purpose of these tests is to ensure that parsers can handle a variety of lockfiles. As such,
    # they may contain invalid dependency graphs which can result in DependencyParserErrors. Since these
    # are out of scope for the parser tests, we ignore them here.
    error = [e for e in error if "Child dependency version not found" not in e.reason]

    # Assert

    # Some of these files either contain incomplete dependency graphs,
    # or have some packages we cannot really make sense of, so we ignore them
    # We include our failures in the error output for informational purposes
    # if target.endswith("osv_parsing/pnpm/commits/pnpm-lock.yaml"):
    #     assert len(error) == 1
    if target.endswith("files/pnpm-lock.yaml"):
        assert len(error) == 1
    elif target.endswith("exotic/pnpm-lock.yaml"):
        assert len(error) == 5
    # elif target.endswith("osv_parsing/pnpm/peer-dependencies-advanced/pnpm-lock.yaml"):
    #     assert len(error) == 1
    elif target.endswith("pnpm-error-key/pnpm-lock.yaml"):
        assert len(error) == 1
    else:
        assert len(error) == 0
    assert len(caplog.records) == 0

    snapshot_deps = [dependency.to_json() for dependency in dependencies]
    snapshot.assert_match(json.dumps(snapshot_deps, indent=2), "dependencies.json")


# Quite awkward. To test that we can handle a target whose toplevel parent
# contains no lockfiles for the language in our rule, we need to _not_ pass in
# a target that begins with "targets", as that dir contains every kind of lockfile
# So we add the keyword arg to run_semgrep and manually do some cd-ing
@pytest.mark.osemfail
def test_no_lockfiles(
    run_semgrep: RunSemgrep, monkeypatch: pytest.MonkeyPatch, tmp_path: Path, snapshot
):
    (tmp_path / "targets").symlink_to(TARGETS_PATH.resolve())
    (tmp_path / "rules").symlink_to(RULES_PATH.resolve())
    monkeypatch.chdir(tmp_path / "targets" / "basic")

    snapshot.assert_match(
        run_semgrep(
            "../../rules/dependency_aware/js-sca.yaml",
            target_name="stupid.js",
            assume_targets_dir=False,
        ).as_snapshot(),
        "results.txt",
    )
