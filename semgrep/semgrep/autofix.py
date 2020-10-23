import logging
import re
from pathlib import Path
from typing import Dict
from typing import List
from typing import Set
from typing import Tuple

from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

logger = logging.getLogger(__name__)

SPLIT_CHAR = "\n"


class Fix:
    def __init__(self, fixed_contents: str, fixed_lines: List[str]):
        self.fixed_contents = fixed_contents
        self.fixed_lines = fixed_lines


def _get_lines(path: Path) -> List[str]:
    contents = path.read_text()
    lines = contents.split(SPLIT_CHAR)
    return lines


def _get_match_context(rule_match: RuleMatch) -> Tuple[int, int, int, int]:
    start_obj = rule_match.start
    start_line = start_obj.get("line", 1) - 1  # start_line is 1 indexed
    start_col = start_obj.get("col", 1) - 1  # start_col is 1 indexed
    end_obj = rule_match.end
    end_line = end_obj.get("line", 1) - 1  # end_line is 1 indexed
    end_col = end_obj.get("col", 1) - 1  # end_line is 1 indexed
    return start_line, start_col, end_line, end_col


def _basic_fix(rule_match: RuleMatch, fix: str) -> Fix:
    p = rule_match.path
    lines = _get_lines(p)

    # get the start and end points
    start_line, start_col, end_line, end_col = _get_match_context(rule_match)

    # break into before, to modify, after
    before_lines = lines[:start_line]
    before_on_start_line = lines[start_line][:start_col]
    after_on_end_line = lines[end_line][end_col:]  # next char after end of match
    modified_lines = (before_on_start_line + fix + after_on_end_line).splitlines()
    after_lines = lines[end_line + 1 :]  # next line after end of match
    contents_after_fix = before_lines + modified_lines + after_lines

    return Fix(SPLIT_CHAR.join(contents_after_fix), modified_lines)


def _regex_replace(
    rule_match: RuleMatch, from_str: str, to_str: str, count: int = 1
) -> Fix:
    """
    Use a regular expression to autofix.
    Replaces from_str to to_str, starting from the left,
    exactly `count` times.
    """
    path = rule_match.path
    lines = _get_lines(path)

    start_line, _, end_line, _ = _get_match_context(rule_match)

    before_lines = lines[:start_line]
    after_lines = lines[end_line + 1 :]

    match_context = lines[start_line : end_line + 1]

    fix = re.sub(from_str, to_str, "\n".join(match_context), count)
    modified_context = fix.splitlines()
    modified_contents = before_lines + modified_context + after_lines

    return Fix(SPLIT_CHAR.join(modified_contents), modified_context)


def _write_contents(path: Path, contents: str) -> None:
    path.write_text(contents)


def apply_fixes(
    rule_matches_by_rule: Dict[Rule, List[RuleMatch]], dryrun: bool = False
) -> None:
    """
        Modify files in place for all files with findings from rules with an
        autofix configuration
    """
    modified_files: Set[Path] = set()

    for _, rule_matches in rule_matches_by_rule.items():
        for rule_match in rule_matches:
            fix = rule_match.fix
            fix_regex = rule_match.fix_regex
            filepath = rule_match.path
            if fix:
                try:
                    fixobj = _basic_fix(rule_match, fix)
                except Exception as e:
                    raise SemgrepError(f"unable to modify file {filepath}: {e}")
            elif fix_regex:
                regex = fix_regex.get("regex")
                replacement = fix_regex.get("replacement")
                count = fix_regex.get("count", 0)
                if not regex or not replacement:
                    raise SemgrepError(
                        "'regex' and 'replacement' values required when using 'fix-regex'"
                    )
                try:
                    count = int(count)
                except ValueError:
                    raise SemgrepError(
                        "optional 'count' value must be an integer when using 'fix-regex'"
                    )
                try:
                    fixobj = _regex_replace(rule_match, regex, replacement, count)
                except Exception as e:
                    raise SemgrepError(
                        f"unable to use regex to modify file {filepath} with fix '{fix}': {e}"
                    )
            else:
                continue
            # endif
            if not dryrun:
                _write_contents(rule_match.path, fixobj.fixed_contents)
                modified_files.add(filepath)
            else:
                rule_match.extra[
                    "fixed_lines"
                ] = fixobj.fixed_lines  # Monkey patch in fixed lines

    num_modified = len(modified_files)
    if len(modified_files):
        logger.info(
            f"successfully modified {num_modified} file{'s' if num_modified > 1 else ''}."
        )
    else:
        logger.info(f"no files modified.")
