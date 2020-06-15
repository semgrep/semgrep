import re
from pathlib import Path
from typing import Dict
from typing import List
from typing import Set
from typing import Tuple

from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.util import print_msg

SPLIT_CHAR = "\n"


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


def _modify_file(rule_match: RuleMatch, fix: str) -> None:
    p = Path(rule_match.path)
    lines = _get_lines(p)

    # get the start and end points
    start_obj = rule_match.start
    start_line = start_obj.get("line", 1) - 1  # start_line is 1 indexed
    start_col = start_obj.get("col", 1) - 1  # start_col is 1 indexed
    end_obj = rule_match.end
    end_line = end_obj.get("line", 1) - 1  # end_line is 1 indexed
    end_col = end_obj.get("col", 1) - 1  # end_line is 1 indexed

    # break into before, to modify, after
    before_lines = lines[:start_line]
    before_on_start_line = lines[start_line][:start_col]
    after_on_end_line = lines[end_line][end_col + 1 :]  # next char after end of match
    modified_lines = (before_on_start_line + fix + after_on_end_line).splitlines()
    after_lines = lines[end_line + 1 :]  # next line after end of match
    contents_after_fix = before_lines + modified_lines + after_lines

    contents_after_fix_str = SPLIT_CHAR.join(contents_after_fix)
    p.write_text(contents_after_fix_str)


def _parse_regex_fix(sed_string: str) -> Tuple[str, str]:
    """
    Return the second and third elements of a sed-like string:
    E.g., s/one/two/g returns (one, two)
    """
    splitstr = sed_string.split("/")[1:3]  # Do it this way to satisfy mypy
    return splitstr[1], splitstr[2]


def _regex_replace(rule_match: RuleMatch, from_str: str, to_str: str) -> None:
    """
    Use a regular expression to autofix.
    Replaces from_str to to_str.
    """
    path = Path(rule_match.path)
    lines = _get_lines(path)

    start_line, start_col, end_line, end_col = _get_match_context(rule_match)

    before_lines = lines[:start_line]
    after_lines = lines[end_line + 1 :]

    match_context_prior = lines[start_line][:start_col]
    match_context_after = lines[end_line][end_col + 1 :]
    match_context = "".join(lines[start_line : end_line + 1])[start_col : end_col + 1]

    fix = re.sub(from_str, to_str, match_context)

    modified_context = (match_context_prior + fix + match_context_after).splitlines()

    modified_contents = before_lines + modified_context + after_lines
    path.write_text(SPLIT_CHAR.join(modified_contents))


def apply_fixes(rule_matches_by_rule: Dict[Rule, List[RuleMatch]]) -> None:
    """
        Modify files in place for all files with findings from rules with an
        autofix configuration
    """
    modified_files: Set[Path] = set()

    for _, rule_matches in rule_matches_by_rule.items():
        for rule_match in rule_matches:
            fix = rule_match.fix
            filepath = rule_match.path
            if fix and fix.startswith("s/"):  # Regex-style fix
                try:
                    from_str, to_str = _parse_regex_fix(fix)
                    _regex_replace(rule_match, from_str, to_str)
                    modified_files.add(filepath)
                except Exception as e:
                    raise SemgrepError(
                        f"unable to use regex to modify file {filepath}: {e}"
                    )
            elif fix:  # Old style fix
                try:
                    _modify_file(rule_match, fix)
                    modified_files.add(filepath)
                except Exception as e:
                    raise SemgrepError(f"unable to modify file {filepath}: {e}")
    num_modified = len(modified_files)
    print_msg(
        f"Successfully modified {num_modified} file{'s' if num_modified > 1 else ''}."
    )
