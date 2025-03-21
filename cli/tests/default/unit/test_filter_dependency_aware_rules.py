from collections import defaultdict
from typing import List

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.rule import Rule
from semgrep.run_scan import filter_dependency_aware_rules
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import ManifestKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pipfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.subproject import from_resolved_dependencies


@pytest.fixture
def sample_rules():
    rules = [
        Rule(
            {
                "id": "rules.ssc-58d96261-a0dd-47e9-bad6-110669fa8c14",
                "r2c-internal-project-depends-on": {
                    "namespace": "pypi",
                    "package": "protobuf",
                    "version": ">=3.0.0",
                },
                "languages": ["python"],
                "patterns": ["pattern"],
            }
        ),
        Rule(
            {
                "id": "rules.ssc-78d96261-a0dd-47e9-bad6-110669fa8c14",
                "r2c-internal-project-depends-on": {
                    "namespace": "pypi",
                    "package": "protobuf",
                    "version": "<=3.0.0",
                },
                "languages": ["python"],
                "patterns": ["pattern"],
            }
        ),
        Rule(
            {
                "id": "rules.ssc-2258d96261-a0dd-47e9-bad6-110669fa8c14",
                "r2c-internal-project-depends-on": {
                    "namespace": "npm",
                    "package": "other-package",
                    "version": ">=1.0.0",
                },
                "languages": ["javascript"],
                "patterns": ["pattern"],
            }
        ),
        Rule(
            {
                "id": "rules.ssc-5108d96261-a0dd-47e9-bad6-110669fa8c14",
                "r2c-internal-project-depends-on": {
                    "namespace": "pypi",
                    "package": "test",
                    "version": ">=1.0.0",
                },
                "languages": ["python"],
                "patterns": ["pattern"],
            }
        ),
    ]

    # Ensure `should_run_on_semgrep_core` is True by adding a key like 'patterns' to `_raw` for each rule
    for rule in rules:
        rule._raw["patterns"] = [
            "pattern"
        ]  # Any valid key that should trigger `should_run_on_semgrep_core`

    return rules


@pytest.fixture
def sample_resolved_deps():
    # Accurate found_dependencies for protobuf and test packages, including
    # empty allowed_hashes
    resolved_dependencies: List[out.ResolvedDependency] = [
        out.ResolvedDependency(
            (
                out.FoundDependency(
                    package="protobuf",
                    version="3.14.0",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),  # Empty allowed_hashes
                    transitivity=out.DependencyKind(out.Direct()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                ),
                None,
            ),
        ),
        out.ResolvedDependency(
            (
                out.FoundDependency(
                    package="test",
                    version="1.16.0",
                    ecosystem=Ecosystem(value=Pypi()),
                    allowed_hashes=defaultdict(list),  # Empty allowed_hashes
                    transitivity=out.DependencyKind(out.Direct()),
                    resolved_url=None,
                    children=None,
                    git_ref=None,
                ),
                None,
            ),
        ),
    ]

    # Create dependency source
    dependency_source = out.DependencySource(
        out.ManifestLockfileDependencySource(
            (
                out.Manifest(ManifestKind(Pipfile()), out.Fpath("Pipfile")),
                out.Lockfile(
                    out.LockfileKind(out.PipfileLock()), out.Fpath("Pipfile.lock")
                ),
            )
        ),
    )

    resolution_method = out.ResolutionMethod(out.LockfileParsing())

    # Create ResolvedSubproject with accurate found_dependencies and resolution_method
    subprojects = [
        out.ResolvedSubproject(
            info=out.Subproject(
                root_dir=out.Fpath("."),
                dependency_source=dependency_source,
                ecosystem=Ecosystem(value=Pypi()),
            ),
            errors=[],
            resolution_method=resolution_method,
            resolved_dependencies=from_resolved_dependencies(resolved_dependencies),
            ecosystem=Ecosystem(value=Pypi()),
        )
    ]

    return {
        Ecosystem(
            value=Pypi()
        ): subprojects  # Ensure the ecosystem is lowercase to match rules
    }


# The actual test function
@pytest.mark.quick
def test_filter_dependency_aware_rules(sample_rules, sample_resolved_deps):
    result = filter_dependency_aware_rules(sample_rules, sample_resolved_deps)
    # Expected result
    expected_result = [
        sample_rules[0],  # Rule with id=rules.ssc-58d96261-a0dd-47e9-bad6-110669fa8c14
        sample_rules[
            3
        ],  # Rule with id=rules.ssc-5108d96261-a0dd-47e9-bad6-110669fa8c14
    ]
    print(result)
    # Assert the result matches the expected output
    assert result == expected_result
