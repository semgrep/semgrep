from collections import defaultdict
from pathlib import Path

import pytest

from semgrep.rule import Rule
from semgrep.run_scan import filter_dependency_aware_rules
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ManifestKind
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pipfile_
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import PackageManagerType
from semgrep.subproject import ResolutionMethod
from semgrep.subproject import ResolvedSubproject


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
    # Accurate found_dependencies for protobuf and test packages, including empty allowed_hashes
    found_dependencies = [
        FoundDependency(
            package="protobuf",
            version="3.14.0",
            ecosystem=Ecosystem(value=Pypi()),
            allowed_hashes=defaultdict(list),  # Empty allowed_hashes
            transitivity=Transitivity("Direct"),
            resolved_url=None,
            children=None,
            git_ref=None,
        ),
        FoundDependency(
            package="test",
            version="1.16.0",
            ecosystem=Ecosystem(value=Pypi()),
            allowed_hashes=defaultdict(list),  # Empty allowed_hashes
            transitivity=Transitivity("Direct"),
            resolved_url=None,
            children=None,
            git_ref=None,
        ),
    ]

    # Create LockfileDependencySource
    dependency_source = LockfileDependencySource(
        manifest=(ManifestKind(value=Pipfile_()), Path("Pipfile")),
        package_manager_type=PackageManagerType.PIP,
        lockfile_path=Path("Pipfile.lock"),
    )

    resolution_method = ResolutionMethod.LOCKFILE_PARSING

    # Create ResolvedSubproject with accurate found_dependencies and resolution_method
    subprojects = [
        ResolvedSubproject.from_unresolved(
            unresolved=ResolvedSubproject(
                root_dir=Path("."),
                ecosystem=Ecosystem(value=Pypi()),
                dependency_source=dependency_source,
                found_dependencies=found_dependencies,
                resolution_method=resolution_method,
            ),
            resolution_method=resolution_method,
            found_dependencies=found_dependencies,
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
