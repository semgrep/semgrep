import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Set
from typing import Tuple
from typing import Union

from rich.progress import MofNCompleteColumn
from rich.progress import Progress
from rich.progress import SpinnerColumn
from rich.progress import TextColumn

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.subproject_matchers import MATCHERS
from semdep.subproject_matchers import SubprojectMatcher
from semgrep.console import console
from semgrep.resolve_dependency_source import resolve_dependency_source
from semgrep.rule import Rule
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_types import Language
from semgrep.subproject import DependencyResolutionConfig
from semgrep.subproject import find_closest_subproject
from semgrep.subproject import from_resolved_dependencies
from semgrep.subproject import get_all_source_files
from semgrep.target_manager import SAST_PRODUCT
from semgrep.target_manager import TargetManager
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


def to_sca_error(
    err: Union[out.DependencyParserError, out.ScaResolutionError]
) -> out.ScaError:
    if isinstance(err, out.DependencyParserError):
        return out.ScaError(out.SCAParse(err))
    elif isinstance(err, out.ScaResolutionError):
        return out.ScaError(out.SCAResol(err))
    else:
        raise TypeError(f"Unexpected error variant: {type(err)}")


@dataclass(frozen=True)
class HashableSubproject:
    """
    A wrapper around Subproject that implements __hash__ and __eq__ to only
    consider root_dir and ecosystem, making it safe to use in sets even when
    the underlying Subproject contains unhashable types like lists.
    Indeed subproject dependency_source field can contain a
    MultilockfileDependencySource with an unhashable list inside.
    """

    subproject: out.Subproject

    def __hash__(self) -> int:
        # Only hash the root_dir and ecosystem
        return hash((self.subproject.root_dir, self.subproject.ecosystem))

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, HashableSubproject):
            return NotImplemented
        return (self.subproject.root_dir, self.subproject.ecosystem) == (
            other.subproject.root_dir,
            other.subproject.ecosystem,
        )


def find_subprojects(
    dependency_source_files: FrozenSet[Path], matchers: List[SubprojectMatcher]
) -> List[out.Subproject]:
    """
    Using the given dependency source files and the given list of matchers,
    return all the subprojects that could be created. Note that each dependency
    source file will be used by at most one matcher, and matching will be
    attempted in the order that the matchers are provided.
    """
    unresolved_subprojects: List[out.Subproject] = []
    used_files: Set[Path] = set()
    for matcher in matchers:
        # for each matcher, pass only those files that have not yet been used
        # by another matcher.
        new_subprojects, new_used_files = matcher.make_subprojects(
            dependency_source_files - used_files
        )
        used_files |= new_used_files
        unresolved_subprojects.extend(new_subprojects)
    return unresolved_subprojects


def filter_changed_subprojects(
    target_manager: TargetManager,
    dependency_aware_rules: List[Rule],
    subprojects: List[out.Subproject],
) -> Tuple[List[out.Subproject], List[out.UnresolvedSubproject]]:
    """
    Partition subprojects into those that are relevant for the targets in
    `target_manager` and those that are not.
    This allows skipping resolution of unchanged subprojects in diff scans.

    Marks irrelevant subprojects' unresolved reason as "skipped".

    Note that the logic used here to determine changed subprojects must be
    consistent with the logic used at finding-generation time in
    `dependency_aware_rule.py` to associate code files with subproject. If we
    do not resolve a subproject because it is deemed irrelevant in this
    function, we will not consider that subproject when generating findings.
    """
    relevant_subprojects: Set[HashableSubproject] = set()

    # first, mark any subprojects whose dependency source files were directly
    # modified as relevant
    all_dependency_source_targets = target_manager.get_all_dependency_source_files(
        ignore_baseline_handler=False
    )
    for subproject in subprojects:
        source_file_set = set(get_all_source_files(subproject.dependency_source))
        if len(all_dependency_source_targets.intersection(source_file_set)) > 0:
            # one of the source files for this subproject changed, so we should keep it
            relevant_subprojects.add(HashableSubproject(subproject))

    if len(relevant_subprojects) == len(subprojects):
        # all subproject are already relevant, so there is no need to look at code files
        # (this should cover the full scan case and prevent extra work)
        # need to refer to the original list for deterministic ordering
        return [
            s for s in subprojects if HashableSubproject(s) in relevant_subprojects
        ], []

    # make language -> ecosystem mapping from the rules that we are given
    ecosystems_by_language: Dict[Language, List[Ecosystem]] = {}
    for rule in dependency_aware_rules:
        for language in rule.languages:
            if language not in ecosystems_by_language:
                ecosystems_by_language[language] = []
            for ecosystem in rule.ecosystems:
                # inefficient lookup, but we need to use a list rather than a set to preserve order and
                # the number of ecosystems per language is small (<10)
                if ecosystem not in ecosystems_by_language[language]:
                    ecosystems_by_language[language].append(ecosystem)

    # note that this logic re-implements the logic in `dependency_aware_rule.py`
    for language, ecosystems in ecosystems_by_language.items():
        for code_file in target_manager.get_files_for_language(
            lang=language, product=SAST_PRODUCT
        ).kept:
            # there may be multiple ecosystems for a single language, and the finding-generation
            # logic will find a different closest subproject for each one. So we need to mark
            # the closest subproject for each relevant ecosystem as potentially changed
            for ecosystem in ecosystems:
                # This is nondeterministic need to fix
                closest_subproject = find_closest_subproject(
                    code_file, ecosystem, subprojects
                )
                if closest_subproject is not None:
                    relevant_subprojects.add(HashableSubproject(closest_subproject))

    # we refer to the original list for ordering, ensuring that the output order
    # is deterministic.
    ordered_relevant = [
        s for s in subprojects if HashableSubproject(s) in relevant_subprojects
    ]
    ordered_irrelevant = [
        s for s in subprojects if HashableSubproject(s) not in relevant_subprojects
    ]
    unresolved_subprojects = [
        out.UnresolvedSubproject(
            info=s, reason=out.UnresolvedReason(out.UnresolvedSkipped()), errors=[]
        )
        for s in ordered_irrelevant
    ]
    return ordered_relevant, unresolved_subprojects


def resolve_subprojects(
    target_manager: TargetManager,
    dependency_aware_rules: List[Rule],
    config: DependencyResolutionConfig,
) -> Tuple[
    List[out.UnresolvedSubproject],
    Dict[Ecosystem, List[out.ResolvedSubproject]],
    List[Path],
]:
    """
    Identify subprojects based on lockfiles and manifests and resolve their
    dependency information.

    The `config` argument controls the behavior of the
    dependency resolver:

    When `allow_local_builds` is False, dependencies are resolved only by
    parsing existing files (lockfiles and manifests).
    If `allow_local_builds` is True, this function may cause projects that
    are scanned to be built. This may involve:
    - Downloading packages from the internet
    - Executing code that is included in the scanned project or in downloaded
      packages

    If `ptt_enabled` is True, dependency resolvers that support dependency
    path will be prioritized.

    If `resolve_untargeted_subprojects` is False, only subprojects with
    dependency source files or relevant code files are resolved and the
    remaining subprojects are skipped.
    If `resolve_untargeted_subprojects` is True, this filtering is disabled and
    resolution is attempted for every found subproject.
    The list of rules is required in order to choose which subprojects to
    resolve and which can be skipped based
    on the set of targets reported by the `target_manager`.

    If `download_source` is True, dependency resolvers will attempt to download
    source code for each subproject's dependencies. For all currently supported
    package managers, this requires that `allow_dynamic_resolution` is also True.

    Returns a tuple with the following items:
        1. Unresolved subprojects
        2. Resolved subprojects, grouped by ecosystem
        4. Dependency source paths that were used in the resolution process
    """
    # first, find all subprojects. We ignore the baseline handler because we want
    # to _identify_, but not necessarily resolve, even unchanged subprojects.
    dependency_source_files = target_manager.get_all_dependency_source_files(
        ignore_baseline_handler=True
    )
    found_subprojects = find_subprojects(dependency_source_files, MATCHERS)

    # A subproject is relevant if one of its dependency source files is a target
    # or there exist a code target for which find_closest_subproject is that
    # subproject.
    if config.resolve_untargeted_subprojects:
        relevant_subprojects = found_subprojects
        irrelevant_subprojects: List[out.UnresolvedSubproject] = []
    else:
        relevant_subprojects, irrelevant_subprojects = filter_changed_subprojects(
            target_manager, dependency_aware_rules, found_subprojects
        )

    # targets that were considered in generating the dependency tree
    dependency_targets: List[Path] = []

    resolved: Dict[Ecosystem, List[out.ResolvedSubproject]] = {}
    unresolved: List[out.UnresolvedSubproject] = irrelevant_subprojects

    # Dispatch each subproject to a resolver for resolution
    with Progress(
        SpinnerColumn(style="green"),
        TextColumn("[bold]{task.description}[/bold]"),
        MofNCompleteColumn(),
        TextColumn("({task.fields[subproject_dir]})"),
        transient=True,
        console=console,
        disable=(not sys.stderr.isatty() or len(relevant_subprojects) == 0),
    ) as progress:
        task_id = progress.add_task(
            "Resolving dependencies", total=len(relevant_subprojects), subproject_dir=""
        )
        for item_i, subproject in enumerate(relevant_subprojects):
            progress.update(task_id, subproject_dir=subproject.root_dir)
            if subproject.ecosystem is None:
                # no reason to resolve subprojects that we don't support. We only recognize them
                # for tracking purposes
                unresolved.append(
                    out.UnresolvedSubproject(
                        info=subproject,
                        reason=out.UnresolvedReason(out.UnresolvedUnsupported()),
                        errors=[],
                    )
                )
                continue
            resolved_info, errors, targets = resolve_dependency_source(
                subproject.dependency_source,
                config,
            )
            dependency_targets.extend(targets)

            if not isinstance(resolved_info, out.UnresolvedReason):
                # resolved_info is an UnresolvedReason when dependency resolution failed in some way
                resolution_method, deps = resolved_info
                resolved_subproject = out.ResolvedSubproject(
                    info=subproject,
                    resolution_method=resolution_method,
                    ecosystem=subproject.ecosystem,
                    resolved_dependencies=from_resolved_dependencies(deps),
                    errors=[to_sca_error(e) for e in errors],
                )

                if resolved_subproject.ecosystem not in resolved:
                    resolved[resolved_subproject.ecosystem] = []
                resolved[resolved_subproject.ecosystem].append(resolved_subproject)
            else:
                # we were not able to resolve the subproject, so track it as an
                # unresolved subproject
                unresolved.append(
                    out.UnresolvedSubproject(
                        info=subproject,
                        reason=resolved_info,
                        errors=[to_sca_error(e) for e in errors],
                    )
                )

            progress.update(task_id, completed=item_i + 1)

        progress.remove_task(task_id)

    return unresolved, resolved, dependency_targets
