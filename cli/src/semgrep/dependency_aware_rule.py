from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Set
from typing import Tuple

import semgrep.output_from_core as core
from semdep.find_lockfiles import find_single_lockfile
from semdep.models import PackageManagers
from semdep.package_restrictions import dependencies_range_match_any
from semdep.package_restrictions import ProjectDependsOnEntry
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.target_manager import TargetManager


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


def generate_unreachable_sca_findings(
    rule: Rule, target_manager: TargetManager, exlcude: Set[Path]
) -> Tuple[List[RuleMatch], List[SemgrepError], Set[Path]]:
    depends_on_keys = rule.project_depends_on
    dep_rule_errors: List[SemgrepError] = []

    depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
    namespaces = list(rule.namespaces)

    non_reachable_matches = []
    targeted_lockfiles = set()
    for namespace in namespaces:
        lockfile_data = target_manager.get_lockfile_dependencies(namespace)
        for lockfile_path, deps in lockfile_data:
            if lockfile_path in exlcude:
                continue
            targeted_lockfiles.add(lockfile_path)
            dependency_matches = list(
                dependencies_range_match_any(depends_on_entries, list(deps))
            )
            for dep_pat, found_dep in dependency_matches:
                json_dep_match = {
                    "dependency_pattern": vars(dep_pat),
                    "found_dependency": vars(found_dep),
                    "lockfile": str(lockfile_path),
                }
                match = RuleMatch(
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
                        "dependency_matches": [json_dep_match],
                    },
                )
                non_reachable_matches.append(match)
    return non_reachable_matches, dep_rule_errors, targeted_lockfiles


def generate_reachable_sca_findings(
    matches: List[RuleMatch], rule: Rule
) -> Tuple[List[RuleMatch], List[SemgrepError], Set[Path]]:
    depends_on_keys = rule.project_depends_on
    dep_rule_errors: List[SemgrepError] = []

    depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
    namespaces = list(rule.namespaces)

    # Reachability rule
    reachable_matches = []
    matched_lockfiles = set()
    for namespace in namespaces:
        for match in matches:
            try:
                lockfile_data = find_single_lockfile(match.path, namespace)
                if lockfile_data is None:
                    continue
                lockfile_path, deps = lockfile_data
                dependency_matches = list(
                    dependencies_range_match_any(depends_on_entries, deps)
                )
                if dependency_matches:
                    matched_lockfiles.add(lockfile_path)
                for dep_pat, found_dep in dependency_matches:
                    json_dep_match = {
                        "dependency_pattern": vars(dep_pat),
                        "found_dependency": vars(found_dep),
                        "lockfile": str(lockfile_path),
                    }
                    match.extra["dependency_match_only"] = False
                    match.extra["dependency_matches"] = [json_dep_match]
                    reachable_matches.append(match)
            except SemgrepError as e:
                dep_rule_errors.append(e)
    return reachable_matches, dep_rule_errors, matched_lockfiles
