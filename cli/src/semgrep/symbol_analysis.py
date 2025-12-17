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
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import UnknownLanguageError
from semgrep.rpc_call import run_symbol_analysis as run_symbol_analysis_rpc
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.subproject import find_closest_subproject
from semgrep.target_manager import SCA_PRODUCT
from semgrep.target_manager import TargetManager
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


@dataclass
class _Language:
    """Use lang.base as the parameter for symbol analysis, but pull
    files in for lang.base + lang.additional.

    This lets us deal with NPM, where we want to specify "ts" as the
    language, but run the code over both .ts *and* .js files (since
    JavaScript is a strict subset of TypeScript).
    """

    base: Language
    additional: Sequence[Language]

    def all_languages(self) -> Iterator[Language]:
        yield self.base
        yield from self.additional


def _ecosystem_to_language(ecosystem: out.Ecosystem) -> Optional[_Language]:
    """
    Converts an ecosystem to a language in a hacky way based off of semgrep_interfaces/lang.json

    There is no construct yet to properly map an ecosystem to a language. Take Maven as an example,
    the language can be java, scala, kotlin, etc. It can be really hard to determine the language
    from the ecosystem.

    If lang is truly an issue, we can modify the ocaml side of things to determine the language per file
    but that involves a lot more work. For now, this is good enough for the SCRAT project.
    """
    kind = ecosystem.kind.lower()

    # We .resolve(id_string) to get the Language object from the LANGUAGE singleton.
    # You can get the id_string from `lang.json`.
    try:
        if kind == "pypi":
            return _Language(LANGUAGE.resolve("python"), [])
        elif kind == "npm":
            return _Language(LANGUAGE.resolve("ts"), [LANGUAGE.resolve("js")])
    except UnknownLanguageError:
        logger.error("Invalid language detected")

    return None


def build_subproject_file_mapping(
    subprojects_by_ecosystem: Mapping[out.Ecosystem, Sequence[out.ResolvedSubproject]],
    target_manager: TargetManager,
) -> Dict[Tuple[out.Ecosystem, Path], List[Path]]:
    """
    Builds a mapping from (ecosystem, subproject_root) -> list of files belonging to that subproject.

    This mimics the logic in resolve_subprojects.py and dependency_aware_rule.py.
    TODO: This should be computed once and passed around to avoid duplicate work.

    Args:
        subprojects_by_ecosystem: Resolved subprojects grouped by ecosystem
        target_manager: Target manager containing all files to analyze

    Returns:
        Mapping from (ecosystem, root_dir) to list of files. The tuple key handles cases where
        the same directory could be a subproject for multiple ecosystems (e.g., monorepos with
        both npm and pypi packages).
    """
    subproject_files = defaultdict[Tuple[out.Ecosystem, Path], List[Path]](list)

    for ecosystem, subprojects in subprojects_by_ecosystem.items():
        ecosystem_lang = _ecosystem_to_language(ecosystem)
        if ecosystem_lang is None:
            continue

        # Get all code files for this language
        for lang in ecosystem_lang.all_languages():
            for code_file in target_manager.get_files_for_language(
                lang=lang, product=SCA_PRODUCT
            ).kept:
                # Find which subproject this file belongs to (note that this logic re-implements the logic in `dependency_aware_rule.py`)
                closest_subproject = find_closest_subproject(
                    code_file,
                    ecosystem,
                    [
                        sp.info for sp in subprojects
                    ],  # convert ResolvedSubproject to Subproject
                )
                if closest_subproject is not None:
                    key = (ecosystem, Path(closest_subproject.root_dir.value))
                    subproject_files[key].append(code_file.fpath)

    return subproject_files


def run_symbol_analysis_for_files(
    root_path: Path,
    lang: str,
    files: List[Path],
) -> Optional[out.SymbolAnalysis]:
    """
    Primitive function to run symbol analysis on a list of files.

    This is the core operation - easy to test in a REPL with simple inputs.

    Args:
        root_path: Root directory of the subproject
        lang: Language to analyze (e.g., "python", "ts")
        files: List of file paths to analyze

    Returns:
        SymbolAnalysis result or None if RPC call fails

    Example usage in REPL:
        >>> from pathlib import Path
        >>> from semgrep.symbol_analysis import run_symbol_analysis_for_files
        >>> result = run_symbol_analysis_for_files(
        ...     root_path=Path("/path/to/project"),
        ...     lang="python",
        ...     files=list(Path("/path/to/project").rglob("*.py"))
        ... )
        >>> print(result.to_json())
    """
    file_fpaths = [out.Fpath(str(f)) for f in files]

    symbol_analysis = run_symbol_analysis_rpc(
        params=out.SymbolAnalysisParams(
            root_path=out.Fpath(str(root_path)),
            lang=lang,
            files=file_fpaths,
        ),
    )

    return symbol_analysis


def run_subproject_symbol_analysis(
    subprojects_by_ecosystem: Mapping[out.Ecosystem, Sequence[out.ResolvedSubproject]],
    target_manager: TargetManager,
) -> out.SymbolAnalysis:
    """
    Runs symbol analysis for all subprojects and returns the combined results.

    This is the high-level orchestration function that:
    1. Builds the file->subproject mapping
    2. Runs symbol analysis for each subproject
    3. Combines all results
    """
    # Build the mapping from subprojects to their files
    # TODO: This should be computed once and passed around to avoid duplicate work.
    # Should replace TargetManager in the arguments.
    subproject_files = build_subproject_file_mapping(
        subprojects_by_ecosystem, target_manager
    )

    combined_symbol_analysis: List[out.SymbolUsage] = []

    for ecosystem, subprojects in subprojects_by_ecosystem.items():
        lang = _ecosystem_to_language(ecosystem)
        if lang is None:
            logger.debug(
                f"No language found for ecosystem {ecosystem}, skipping SCA symbol analysis"
            )
            continue

        for subproject in subprojects:
            key = (ecosystem, Path(subproject.info.root_dir.value))
            files = subproject_files.get(key, [])

            if not files:
                logger.debug(
                    f"No files found for subproject {subproject.info.root_dir}, skipping"
                )
                continue

            symbol_analysis = run_symbol_analysis_for_files(
                root_path=Path(subproject.info.root_dir.value),
                lang=lang.base,
                files=files,
            )

            if symbol_analysis is None:
                continue

            combined_symbol_analysis.extend(symbol_analysis.value)

    return out.SymbolAnalysis(value=combined_symbol_analysis)
