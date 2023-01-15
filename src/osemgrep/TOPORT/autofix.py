class Fix:
    def __init__(self, fixed_contents: str, fixed_lines: List[str]):
        self.fixed_contents = fixed_contents
        self.fixed_lines = fixed_lines

class FileOffsets:
    """
    This object is used to track state when applying multiple fixes to the same
    or subsequent lines in a single file.

    The assumption and current state here is that fixes are applied top-to-
    bottom and in a single pass; semgrep will not come back and re-parse or
    re-fix a file or line. This approach may need to be revisited or extended
    to support more complex overlapping autofix cases in future.
    """

    def __init__(self, line_offset: int, col_offset: int, active_line: int):
        self.line_offset = line_offset
        self.col_offset = col_offset
        self.active_line = active_line


def _get_match_context(
    rule_match: RuleMatch, offsets: FileOffsets
) -> Tuple[int, int, int, int]:
    start_obj = rule_match.start
    start_line = start_obj.line - 1  # start_line is 1 indexed
    start_col = start_obj.col - 1  # start_col is 1 indexed
    end_obj = rule_match.end
    end_line = end_obj.line - 1  # end_line is 1 indexed
    end_col = end_obj.col - 1  # end_line is 1 indexed

    # adjust based on offsets
    start_line = start_line + offsets.line_offset
    end_line = end_line + offsets.line_offset
    start_col = start_col + offsets.col_offset
    end_col = end_col + offsets.col_offset

    return start_line, start_col, end_line, end_col


def _regex_replace(
    rule_match: RuleMatch,
    file_offsets: FileOffsets,
    from_str: str,
    to_str: str,
    count: int = 1,
) -> Tuple[Fix, FileOffsets]:
    """
    Use a regular expression to autofix.
    Replaces from_str to to_str, starting from the left,
    exactly `count` times.
    """
    path = rule_match.path
    lines = _get_lines(path)

    start_line, _, end_line, _ = _get_match_context(rule_match, file_offsets)

    before_lines = lines[:start_line]
    after_lines = lines[end_line + 1 :]

    match_context = lines[start_line : end_line + 1]

    fix = re.sub(from_str, to_str, "\n".join(match_context), count)
    modified_context = fix.splitlines()
    modified_contents = before_lines + modified_context + after_lines

    # update offsets
    file_offsets.line_offset = len(modified_context) - len(match_context)

    return Fix(SPLIT_CHAR.join(modified_contents), modified_context), file_offsets


def apply_fixes(rule_matches_by_rule: RuleMatchMap, dryrun: bool = False) -> None:
    """
    Modify files in place for all files with findings from rules with an
    autofix configuration
    """
    modified_files: Set[Path] = set()
    modified_files_offsets: Dict[Path, FileOffsets] = {}
    for _, rule_matches in rule_matches_by_rule.items():
        for rule_match in rule_matches:
            fix = rule_match.fix
            fix_regex = rule_match.fix_regex
            filepath = rule_match.path
            # initialize or retrieve/update offsets for the file
            file_offsets = modified_files_offsets.get(
                filepath, FileOffsets(0, 0, rule_match.start.line)
            )
            if file_offsets.active_line != rule_match.start.line:
                file_offsets.active_line = rule_match.start.line
                file_offsets.col_offset = 0
            if fix:
                try:
                    fixobj, new_file_offset = _basic_fix(rule_match, file_offsets, fix)
                except Exception as e:
                    raise SemgrepError(f"unable to modify file {filepath}: {e}")
            elif fix_regex:
                regex = fix_regex.regex
                replacement = fix_regex.replacement
                count = fix_regex.count or 0
                try:
                    fixobj, new_file_offset = _regex_replace(
                        rule_match, file_offsets, regex, replacement, count
                    )
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
                modified_files_offsets[filepath] = new_file_offset
            else:
                rule_match.extra[
                    "fixed_lines"
                ] = fixobj.fixed_lines  # Monkey patch in fixed lines
