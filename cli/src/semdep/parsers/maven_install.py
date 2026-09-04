#
# Copyright (c) 2026 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
"""
Parser for maven_install.json lockfiles produced by rules_jvm_external.

rules_jvm_external is the standard way to pull Maven dependencies into a Bazel
workspace. When `maven_install(...)` is called with a pinned artifact file,
the generated JSON is a deterministic lockfile listing every resolved Maven
coordinate, its transitive graph, and per-artifact SHA-256 checksums.

Two on-disk formats are supported, matching the shapes rules_jvm_external has
shipped historically:

  * **Legacy `0.1.0`** — a single top-level `dependency_tree` object with a
    `dependencies` LIST whose entries carry `coord` (a three-part
    `group:artifact:version`), `sha256`, `url`, and a nested `dependencies`
    list of transitive `group:artifact` neighbours. Emitted by older
    rules_jvm_external releases (3.x and earlier). See:
    https://github.com/bazel-contrib/rules_jvm_external/blob/master/private/rules/v1_lock_file.bzl

  * **Current `3`** — a flat top-level object with two MAPS:
    `artifacts` (keyed by `group:artifact`, value `{version, shasums.jar}`)
    and `dependencies` (adjacency map keyed by `group:artifact` with a list
    of transitive neighbours). Emitted by rules_jvm_external 4.x and later.

In both formats, workspace-declared root artifacts appear as keys of
`__INPUT_ARTIFACTS_HASH` (excluding the reserved `"repositories"` entry).
That map is our source of truth for direct-vs-transitive classification.
If it is absent for any reason, dependencies are emitted with `Unknown`
transitivity rather than a guess.

Coordinate normalization:
  Semgrep's Maven advisory matching keys on `group:artifact`. Version-3
  artifact keys are already in that shape. Legacy 0.1.0 `coord` values may
  include packaging and/or classifier segments
  (`group:artifact[:packaging[:classifier]]:version`), which we drop.

References:
  - https://github.com/bazel-contrib/rules_jvm_external
  - https://github.com/bazel-contrib/rules_jvm_external/blob/master/docs/pinning-artifacts.md
"""
from pathlib import Path
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semgrep import telemetry
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def _split_gav_coord(coord: str) -> Optional[Tuple[str, str]]:
    """
    Split a rules_jvm_external legacy `coord` into (package, version).

    Accepted shapes (0.1.0 format `coord` field):
      group:artifact:version
      group:artifact:packaging:version
      group:artifact:packaging:classifier:version

    Semgrep's Maven advisories key on "group:artifact"; packaging/classifier
    are dropped. Returns None for unparseable coordinates.
    """
    parts = coord.split(":")
    if len(parts) < 3:
        return None
    group, artifact, version = parts[0], parts[1], parts[-1]
    if not group or not artifact or not version:
        return None
    return f"{group}:{artifact}", version


def _direct_roots(scope: JSON) -> Optional[FrozenSet[str]]:
    """
    Return the set of workspace-declared direct-dependency keys.

    rules_jvm_external records the artifacts that appeared in the workspace's
    `artifacts = [...]` argument as keys of `__INPUT_ARTIFACTS_HASH` (the
    reserved key `"repositories"` is filtered out). Returns None when the
    field is absent, which the caller should interpret as "directness cannot
    be determined."
    """
    try:
        scope_dict = scope.as_dict()
    except Exception:
        return None
    hash_field = scope_dict.get("__INPUT_ARTIFACTS_HASH")
    if hash_field is None:
        return None
    try:
        return frozenset(k for k in hash_field.as_dict().keys() if k != "repositories")
    except Exception:
        return None


def _transitivity(
    package: str, direct_roots: Optional[FrozenSet[str]]
) -> out.DependencyKind:
    """Direct if `package` is in the workspace's declared roots; else Transitive.
    Unknown when we could not extract direct-root information at all."""
    if direct_roots is None:
        return out.DependencyKind(Unknown())
    if package in direct_roots:
        return out.DependencyKind(Direct())
    return out.DependencyKind(Transitive())


def _sha256_from_shasums(shasums_json: JSON) -> Optional[str]:
    """Extract a sha256 hex from a v3 `shasums` object.

    rules_jvm_external usually keys the primary checksum under `"jar"`, even
    for packaging types like `aar`. When multiple classifiers are present
    (e.g. `{jar, test-fixtures}`) we prefer the `jar` entry explicitly rather
    than relying on iteration order; if `jar` is absent we fall back to any
    single available entry.
    """
    try:
        entries = shasums_json.as_dict()
    except Exception:
        return None
    jar_json = entries.get("jar")
    if jar_json is not None:
        try:
            s = jar_json.as_str()
            if s:
                return s
        except Exception:
            pass
    for _classifier, sum_json in entries.items():
        try:
            s = sum_json.as_str()
        except Exception:
            continue
        if s:
            return s
    return None


def _normalize_v3_artifact_key(key: str) -> Optional[str]:
    """Normalize a v3 `artifacts` map key to `group:artifact`.

    Real rules_jvm_external v3 lockfiles key the `artifacts` map by
    `group:artifact` or `group:artifact:packaging` (Android `aar` deps, for
    example, appear as `androidx.core:core:aar`). Semgrep's Maven advisories
    match on the two-segment `group:artifact` shape, so drop any trailing
    packaging/classifier segments. `__INPUT_ARTIFACTS_HASH` keys are always
    already in the two-segment shape, so the same normalization is used for
    direct-root lookup.

    Returns None for shapes that can't be a Maven coordinate (missing group,
    missing artifact, or a single segment).
    """
    parts = key.split(":")
    if len(parts) < 2:
        return None
    group, artifact = parts[0], parts[1]
    if not group or not artifact:
        return None
    return f"{group}:{artifact}"


def _parse_v01_legacy(
    lockfile_path: Path,
    manifest_path: Optional[Path],
    dep_tree: JSON,
) -> List[FoundDependency]:
    """Parse the legacy 0.1.0 `dependency_tree.dependencies` LIST format."""
    try:
        dep_tree_dict = dep_tree.as_dict()
    except Exception:
        return []
    deps_field = dep_tree_dict.get("dependencies")
    if deps_field is None:
        return []

    direct_roots = _direct_roots(dep_tree)

    output: List[FoundDependency] = []
    try:
        entries = deps_field.as_list()
    except Exception:
        return []
    for dep_json in entries:
        try:
            dep_dict = dep_json.as_dict()
        except Exception:
            continue
        coord_json = dep_dict.get("coord")
        if coord_json is None:
            continue
        parsed = _split_gav_coord(coord_json.as_str())
        if parsed is None:
            logger.info(f"Skipping unparseable Maven coordinate: {coord_json.as_str()}")
            continue
        package, version = parsed

        allowed_hashes = {}
        sha256_json = dep_dict.get("sha256")
        if sha256_json is not None:
            try:
                sha256 = sha256_json.as_str()
                if sha256:
                    allowed_hashes = {"sha256": [sha256]}
            except Exception:
                pass

        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Maven()),
                allowed_hashes=allowed_hashes,
                transitivity=_transitivity(package, direct_roots),
                line_number=dep_json.line_number,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
    return output


def _parse_v3_flat(
    lockfile_path: Path,
    manifest_path: Optional[Path],
    root: JSON,
) -> List[FoundDependency]:
    """Parse the current-format flat top-level `artifacts` MAP."""
    try:
        root_dict = root.as_dict()
    except Exception:
        return []
    artifacts_field = root_dict.get("artifacts")
    if artifacts_field is None:
        return []

    direct_roots = _direct_roots(root)

    output: List[FoundDependency] = []
    try:
        artifacts = artifacts_field.as_dict()
    except Exception:
        return []
    for key, val_json in artifacts.items():
        # Keys may be `group:artifact` or `group:artifact:packaging` (Android
        # `aar` deps look like `androidx.core:core:aar`). Normalize to two
        # segments so Maven advisory matching lines up.
        package = _normalize_v3_artifact_key(key) if key else None
        if package is None:
            logger.info(f"Skipping unexpected artifact key: {key!r}")
            continue
        try:
            val = val_json.as_dict()
        except Exception:
            continue
        version_json = val.get("version")
        if version_json is None:
            continue
        try:
            version = version_json.as_str()
        except Exception:
            continue
        if not version:
            continue

        allowed_hashes = {}
        shasums_json = val.get("shasums")
        if shasums_json is not None:
            sha256 = _sha256_from_shasums(shasums_json)
            if sha256:
                allowed_hashes = {"sha256": [sha256]}

        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Maven()),
                allowed_hashes=allowed_hashes,
                # Look up direct-root membership using the normalized shape;
                # __INPUT_ARTIFACTS_HASH keys are always `group:artifact`
                # regardless of the packaging suffix on the artifacts key.
                transitivity=_transitivity(package, direct_roots),
                line_number=val_json.line_number,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
    return output


@telemetry.trace(telemetry.TraceOwner.SSC)
def parse_maven_install(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, _parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(out.PJsondoc())),
        None,
    )
    if not parsed_lockfile:
        return [], errors

    # A valid JSON document isn't guaranteed to be an object — a truncated or
    # hand-edited maven_install.json could be an array, string, number, bool,
    # or null. semdep's JSON wrapper's `as_dict()` passes through whatever
    # underlying value is present, so check the returned type explicitly (and
    # defensively catch any exception) rather than letting a downstream
    # `.get()` raise into the caller.
    try:
        root_dict = parsed_lockfile.as_dict()
    except Exception:
        root_dict = None
    if not isinstance(root_dict, dict):
        logger.warning(
            f"{lockfile_path}: top-level JSON is not an object; not a "
            "recognizable rules_jvm_external maven_install.json"
        )
        return [], errors

    # 0.1.0 legacy format: single top-level `dependency_tree` object.
    dep_tree = root_dict.get("dependency_tree")
    if dep_tree is not None:
        return _parse_v01_legacy(lockfile_path, manifest_path, dep_tree), errors

    # Version 3 (current): flat top-level with `artifacts` map.
    # Presence of both `artifacts` and `dependencies` at the top level marks
    # the flat form and distinguishes it from the legacy shape.
    if (
        root_dict.get("artifacts") is not None
        and root_dict.get("dependencies") is not None
    ):
        return _parse_v3_flat(lockfile_path, manifest_path, parsed_lockfile), errors

    logger.warning(
        f"{lockfile_path}: no `dependency_tree` or top-level `artifacts` / "
        "`dependencies` fields; not a recognizable rules_jvm_external "
        "maven_install.json"
    )
    return [], errors
