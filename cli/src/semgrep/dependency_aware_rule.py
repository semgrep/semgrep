from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Tuple

import semgrep.output_from_core as core
from semdep.find_lockfiles import DependencyTrie
from semdep.models import LockfileDependency
from semdep.models import PackageManagers
from semdep.package_restrictions import dependencies_range_match_any
from semdep.package_restrictions import ProjectDependsOnEntry
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


PACKAGE_MANAGER_MAP = {
    "pypi": PackageManagers.PYPI,
    "npm": PackageManagers.NPM,
    "gem": PackageManagers.GEM,
    "gomod": PackageManagers.GOMOD,
    "cargo": PackageManagers.CARGO,
    "maven": PackageManagers.MAVEN,
    "gradle": PackageManagers.GRADLE,
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
    dep_trie: DependencyTrie,
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Run a dependency aware rule. These rules filters the results based on the precense or absence
    of dependencies. Dependencies are determined by searching for lockfiles under the first entry
    in the `targets` argument.
    """
    depends_on_keys = rule.project_depends_on
    dep_rule_errors: List[SemgrepError] = []

    if len(depends_on_keys) == 0:
        # no dependencies to process, so skip
        return matches, []

    depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
    final_matches: List[RuleMatch] = []

    namespaces = rule.namespaces
    dependencies: List[Tuple[Path, List[LockfileDependency]]] = []
    for ns in namespaces:
        if ns in dep_trie.all_deps:
            dependencies.extend(dep_trie.all_deps[ns].items())

    for lockfile_path, deps in dependencies:
        try:
            output = list(
                dependencies_range_match_any(depends_on_entries, lockfile_path, deps)
            )
            if not output:
                continue

            output_for_json = [
                {
                    "dependency_pattern": vars(dep_pat),
                    "found_dependency": vars(found_dep),
                    "lockfile": str(lockfile),
                }
                for dep_pat, found_dep, lockfile in output
            ]

            reachable = []
            matches_remaining = []
            for match in matches:
                if lockfile_path in (dep_trie.find_dependencies(match.path) or {}):
                    match.extra["dependency_match_only"] = False
                    match.extra["dependency_matches"] = output_for_json
                    reachable.append(match)
                else:
                    matches_remaining.append(match)
            matches = matches_remaining
            if not reachable:
                dependency_only_match = RuleMatch(
                    message=rule.message,
                    metadata=rule.metadata,
                    severity=rule.severity,
                    fix=None,
                    fix_regex=None,
                    match=core.CoreMatch(
                        rule_id=core.RuleId(rule.id),
                        location=core.Location(
                            path=str(lockfile_path),
                            start=core.Position(0, 0, 0),
                            end=core.Position(0, 0, 0),
                        ),
                        # TODO: we need to define the fields below in
                        # Output_from_core.atd so we can reuse core.MatchExtra
                        extra=core.CoreMatchExtra(metavars=core.Metavars({})),
                    ),
                    extra={
                        "dependency_match_only": True,
                        "dependency_matches": output_for_json,
                    },
                )
                final_matches.append(dependency_only_match)
            else:
                final_matches.extend(reachable)

        except SemgrepError as e:
            dep_rule_errors.append(e)
    return final_matches, dep_rule_errors
