#
# Copyright (c) 2025 Semgrep Inc.
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
from typing import List

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.subproject import SubprojectKind
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

PTT_OCAML_PARSER_SUBPROJECT_KINDS: List[SubprojectKind] = [
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.NpmPackageLockJson())),
    (out.ManifestKind(out.Csproj()), out.LockfileKind(out.NugetPackagesLockJson())),
    (out.ManifestKind(out.BuildGradle()), out.LockfileKind(out.GradleLockfile())),
    (out.ManifestKind(out.BuildGradleKts()), out.LockfileKind(out.GradleLockfile())),
    # TODO: (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.YarnLock())),
]

# subproject kinds that can only be resolved via dynamic resolution. Generally
# these are subprojects where we have no lockfile available.
ALWAYS_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS: List[SubprojectKind] = [
    (out.ManifestKind(out.PomXml()), None),
    (out.ManifestKind(out.BuildGradle()), None),
    (out.ManifestKind(out.SettingsGradle()), None),
    (
        out.ManifestKind(out.SetupPy()),
        None,
    ),
    (out.ManifestKind(out.Csproj()), None),
]

# Subproject kinds that we use ocaml parsers for only when dependency paths are
# enabled.
PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS: List[SubprojectKind] = [
    (
        out.ManifestKind(out.RequirementsIn()),
        out.LockfileKind(out.PipRequirementsTxt()),
    ),
    (
        None,
        out.LockfileKind(out.PipRequirementsTxt()),
    ),
    (
        out.ManifestKind(out.Pipfile()),
        out.LockfileKind(out.PipfileLock()),
    ),
]

# Subproject kinds that we use ocaml parsers for only when transitive reachability is
# enabled.
TR_OCAML_RESOLVER_SUBPROJECT_KINDS: List[SubprojectKind] = [
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.NpmPackageLockJson())),
    (out.ManifestKind(out.PyprojectToml()), out.LockfileKind(out.UvLock())),
    (out.ManifestKind(out.PyprojectToml()), out.LockfileKind(out.PoetryLock())),
    (None, out.LockfileKind(out.PipRequirementsTxt())),
    (out.ManifestKind(out.Pipfile()), out.LockfileKind(out.PipfileLock())),
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.PnpmLock())),
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.YarnLock())),
]

# Subproject kinds that we use ocaml parsers for always.
ALWAYS_OCAML_PARSER_SUBPROJECT_KINDS: List[SubprojectKind] = [
    (out.ManifestKind(out.PyprojectToml()), out.LockfileKind(out.UvLock()))
]

# All subproject kinds that are supported for transitive reachability, regardless
# of how they are parsed. In practice this is probably identical to TR_OCAML_RESOLVER_SUBPROJECT_KINDS,
# but doesn't necessarily have to be.
# TODO: When TR is optimized enough to just attempt the TR RPC for every subproject,
# remove this list and just try TR all the time.
TRANSITIVE_REACHABILITY_SUBPROJECT_KINDS: List[SubprojectKind] = [
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.NpmPackageLockJson())),
    (out.ManifestKind(out.PyprojectToml()), out.LockfileKind(out.UvLock())),
    (out.ManifestKind(out.PyprojectToml()), out.LockfileKind(out.PoetryLock())),
    (None, out.LockfileKind(out.PipRequirementsTxt())),
    (out.ManifestKind(out.Pipfile()), out.LockfileKind(out.PipfileLock())),
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.PnpmLock())),
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.YarnLock())),
]
