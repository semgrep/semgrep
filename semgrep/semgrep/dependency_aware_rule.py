from os import getenv
from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Tuple

from dependencyparser.find_lockfiles import DependencyTrie
from dependencyparser.find_lockfiles import make_dependency_trie
from dependencyparser.models import PackageManagers
from dependencyparser.package_restrictions import dependencies_range_match_any
from dependencyparser.package_restrictions import ProjectDependsOnEntry
from dependencyparser.parse_lockfile import EXTENSION_TO_LOCKFILES

from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import CoreLocation
from semgrep.rule_match import RuleMatch


PACKAGE_MANAGER_MAP = {
    "pypi": PackageManagers.PYPI,
    "npm": PackageManagers.NPM,
    "gem": PackageManagers.GEM,
    "gomod": PackageManagers.GOMOD,
    "cargo": PackageManagers.CARGO,
    "maven": PackageManagers.MAVEN,
}


def parse_depends_on_yaml(
    entries: List[Dict[str, str]]
) -> Generator[ProjectDependsOnEntry, None, None]:
    """
    Convert the entries in the Yaml to ProjectDependsOnEntry objects that specify
    namespace, package name, and semver ranges
    """
    for entry in entries:
        # schema checks should gaurantee we have these fields, but we'll code defensively
        namespace = entry.get("namespace")
        if namespace is None:
            raise SemgrepError(f"project-depends-on is missing `namespace`")
        pm = PACKAGE_MANAGER_MAP.get(namespace)
        if pm is None:
            raise SemgrepError(
                f"unknown package namespace: {namespace}, only {list(PACKAGE_MANAGER_MAP.keys())} are supported"
            )
        package_name = entry.get("package")
        if package_name is None:
            raise SemgrepError(f"project-depends-on is missing `package`")
        semver_range = entry.get("version")
        if semver_range is None:
            raise SemgrepError(f"project-depends-on is missing `version`")
        yield ProjectDependsOnEntry(
            namespace=pm, package_name=package_name, semver_range=semver_range
        )


def run_dependency_aware_rule(
    matches: List[RuleMatch],
    rule: Rule,
    top_level_target: Path,
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Run a dependency aware rule. These rules filters the results based on the precense or absence
    of dependencies. Dependencies are determined by searching for lockfiles under the first entry
    in the `targets` argument.
    """
    dependencies: List[Dict[str, str]] = rule.project_depends_on or []
    dep_rule_errors: List[SemgrepError] = []

    if len(dependencies) == 0:
        # no dependencies to process, so skip
        return matches, []

    dep_trie = make_dependency_trie(top_level_target)

    depends_on_entries = list(parse_depends_on_yaml(dependencies))
    final_matches = []

    if not rule.should_run_on_semgrep_core:
        # This was a rule with no patterns
        # matches should be empty
        for lockfile_path, deps in dep_trie.all_deps.items():
            try:
                output = list(
                    dependencies_range_match_any(
                        depends_on_entries, lockfile_path, deps
                    )
                )
                if not output:
                    continue
                output_for_json = [
                    {
                        "dependency_pattern": vars(dep_pat),
                        "found_dependency": vars(found_dep),
                        "lockfile": lockfile.name,
                    }
                    for dep_pat, found_dep, lockfile in output
                ]

                dummy_match = RuleMatch(
                    rule_id=rule.id,
                    message=rule.message,
                    metadata=rule.metadata,
                    severity=rule.severity,
                    path=lockfile_path,
                    fix=None,
                    fix_regex=None,
                    start=CoreLocation(0, 0, 0),
                    end=CoreLocation(0, 0, 0),
                    extra={
                        "dependency_match_only": True,
                        "dependency_matches": output_for_json,
                    },
                )

                final_matches.append(dummy_match)
            except SemgrepError as e:
                dep_rule_errors.append(e)

        return final_matches, dep_rule_errors

    for match in matches:
        try:
            all_deps = dep_trie.find_dependencies(match.path) or {}
            lang_lockfiles = EXTENSION_TO_LOCKFILES.get(match.path.suffix)
            if not lang_lockfiles:
                raise SemgrepError(
                    f"We cannot scan lockfiles for the language with extension: {match.path.suffix}"
                )

            lang_deps = [
                (key, all_deps[key])
                for key in all_deps
                if key.parts[-1].lower() in lang_lockfiles
            ]

            if not lang_deps:
                # No dependencies to scan at all
                final_matches.append(match)
                continue

            if len(lang_deps) > 1:
                raise SemgrepError(
                    f"Multiple different lockfile formats found for file {match.path}?"
                )

            [(lockfile_path, deps)] = lang_deps

            output = list(
                dependencies_range_match_any(depends_on_entries, lockfile_path, deps)
            )

            if not output:
                continue

            output_for_json = [
                {
                    "dependency_pattern": vars(dep_pat),
                    "found_dependency": vars(found_dep),
                    "lockfile": lockfile.name,
                }
                for dep_pat, found_dep, lockfile in output
            ]
            match.extra["dependency_match_only"] = False
            match.extra["dependency_matches"] = output_for_json
            final_matches.append(match)
        except SemgrepError as e:
            dep_rule_errors.append(e)
    return final_matches, dep_rule_errors
