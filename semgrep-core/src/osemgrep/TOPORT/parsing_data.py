import os
from dataclasses import dataclass
from typing import Dict
from typing import Tuple
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from semgrep.core_runner import Plan

import semgrep.output_from_core as core
from semgrep.semgrep_types import Language


@dataclass
class LanguageParseData:
    targets_with_errors: int
    num_targets: int
    error_bytes: int
    num_bytes: int


class ParsingData:
    def __init__(self) -> None:
        self._file_info: Dict[str, Tuple[Language, bool]] = {}
        self._parse_errors_by_lang: Dict[Language, LanguageParseData] = {}

    def add_targets(self, plan: "Plan") -> None:
        """
        Adds the targets from a given plan to the set of files tracked for
        parsing statistics
        """
        for task in plan.target_mappings:
            if task.language in ["regex", "generic"]:
                continue
            self._file_info[task.path] = (task.language, True)
            entry = self._parse_errors_by_lang.get(
                task.language, LanguageParseData(0, 0, 0, 0)
            )
            try:
                entry.num_bytes += os.path.getsize(task.path)
                entry.num_targets += 1
            except OSError:
                # Don't count the target if the path doesn't exist
                pass
            self._parse_errors_by_lang[task.language] = entry

    def add_error(self, err: core.CoreError) -> None:
        """
        Records the targets/bytes which were not parsed as a result of the
        given error. The file the error originated from should have been
        registered from the original plan with `add_targets`.
        """
        path = err.location.path
        try:
            (lang, no_error_yet) = self._file_info[path]
        except KeyError:
            # We may be told to register an error for a file we didn't know
            # about from the plan. This can occur due to rules that involve
            # generating new targets, namely ones involving extract mode or
            # metavariable-pattern. In this case, just don't report the parsing
            # statistics for this right now.
            return
        lang_parse_data = self._parse_errors_by_lang[lang]
        if no_error_yet:
            lang_parse_data.targets_with_errors += 1

        # The error types checked here should comprise the parse related errors
        # listed in semgrep_output_v0.atd. If you add a new parse error there,
        # it should get reflected here.
        #
        # Total errors for the whole file
        if isinstance(
            err.error_type.value,
            (
                core.LexicalError,
                core.ParseError,
                core.SpecifiedParseError,
                core.AstBuilderError,
            ),
        ):
            try:
                lang_parse_data.error_bytes += os.path.getsize(path)
            except OSError:
                # In the case that this path is no longer a valid path to a
                # file, just report the original bytes as okay for metrics
                # purposes.
                pass
        # Partial errors for a subsection of the file
        elif isinstance(err.error_type.value, core.PartialParsing):
            for loc in err.error_type.value.value:
                lang_parse_data.error_bytes += loc.end.offset - loc.start.offset
        else:
            # This function should only be called to add information about
            # parsing related errors.
            raise TypeError

    def get_errors_by_lang(self) -> Dict[Language, LanguageParseData]:
        return self._parse_errors_by_lang
