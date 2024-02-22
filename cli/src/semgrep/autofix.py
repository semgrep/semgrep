from functools import cmp_to_key
from pathlib import Path
from typing import Iterable
from typing import List
from typing import Set
from typing import Tuple

from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import OrderedRuleMatchList
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

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
    start_line = start_obj.line - 1  # start_line is 1 indexed
    start_col = start_obj.col - 1  # start_col is 1 indexed
    end_obj = rule_match.end
    end_line = end_obj.line - 1  # end_line is 1 indexed
    end_col = end_obj.col - 1  # end_line is 1 indexed

    return start_line, start_col, end_line, end_col


def _apply_fix(rule_match: RuleMatch, fix: str) -> Fix:
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


def _write_contents(path: Path, contents: str) -> None:
    path.write_text(contents)


def matches_compare(x: RuleMatch, y: RuleMatch) -> int:
    # If paths are not the same, just order based on that.
    # I just need a total ordering on matches.
    if x.path < y.path:
        return -1
    elif x.path > y.path:
        return 1
    else:
        if x.start.offset < y.start.offset:
            return -1
        elif y.start.offset < x.start.offset:
            return 1
        else:
            if x.end.offset < y.end.offset:
                return -1
            elif x.end.offset > y.end.offset:
                return 1
            else:
                return 0


def matches_overlap(x: RuleMatch, y: RuleMatch) -> bool:
    if x.path == y.path:
        if x.start.offset < y.start.offset:
            return x.end.offset > y.start.offset
        elif y.start.offset < x.start.offset:
            return y.end.offset > x.start.offset
        elif x.start.offset == y.start.offset:
            return True

    # If they are not from the same file, they cannot overlap.
    return False


def deduplicate_overlapping_matches(
    rules_and_matches: Iterable[Tuple[Rule, OrderedRuleMatchList]]
) -> OrderedRuleMatchList:
    final_matches = []

    ordered_matches = sorted(
        (match for _, matches in rules_and_matches for match in matches),
        key=cmp_to_key(matches_compare),
    )
    acc = None

    for match in ordered_matches:
        if acc is None:
            acc = match
            continue

        if matches_overlap(acc, match):
            logger.debug(
                f"Two autofix matches from rules {acc.rule_id} and {match.rule_id} overlap, arbitrarily picking first one"
            )
            # Don't do anything, keep `acc` the same, and throw `match` out.`

        else:
            final_matches.append(acc)
            acc = match

    if acc is not None:
        final_matches.append(acc)

    return final_matches


def apply_fixes(rule_matches_by_rule: RuleMatchMap, dryrun: bool = False) -> None:
    """
    Modify files in place for all files with findings from rules with an
    autofix configuration
    """
    modified_files: Set[Path] = set()

    nonoverlapping_matches = deduplicate_overlapping_matches(
        rule_matches_by_rule.items()
    )
    # The deduplication step also sorts the matches. Reverse them here and apply
    # in reverse order. This way we don't have to keep track of changes made
    # earlier in the file when applying each autofix.
    nonoverlapping_matches.reverse()

    for rule_match in nonoverlapping_matches:
        fix = rule_match.fix
        filepath = rule_match.path
        if fix is not None:
            try:
                fixobj = _apply_fix(rule_match, fix)
            except Exception as e:
                raise SemgrepError(f"unable to modify file {filepath}: {e}")
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

    if not dryrun:
        if len(modified_files):
            logger.info(
                f"successfully modified {unit_str(len(modified_files), 'file')}."
            )
        else:
            logger.info(f"no files modified.")
