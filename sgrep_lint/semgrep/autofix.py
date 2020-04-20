from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Set

from semgrep.rule import Rule
from semgrep.util import print_error_exit
from semgrep.util import print_msg


def _generate_fix(rule: Rule, sgrep_result: Dict[str, Any]) -> Optional[Any]:
    # TODO add fix to rule object
    fix_str = rule.raw.get("fix")
    if fix_str is None:
        return None
    if "metavars" in sgrep_result["extra"]:
        for metavar, contents in sgrep_result["extra"]["metavars"].items():
            fix_str = fix_str.replace(metavar, contents["abstract_content"])
    return fix_str


def _modify_file(finding: Dict[str, Any]) -> None:
    p = Path(finding["path"])
    SPLIT_CHAR = "\n"
    contents = p.read_text()
    lines = contents.split(SPLIT_CHAR)
    fix = finding.get("extra", {}).get("fix")

    # get the start and end points
    start_obj = finding.get("start", {})
    start_line = start_obj.get("line", 1) - 1  # start_line is 1 indexed
    start_col = start_obj.get("col", 1) - 1  # start_col is 1 indexed
    end_obj = finding.get("end", {})
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


def apply_fixes(findings_by_rule: Dict[Rule, List[Dict[str, Any]]]) -> None:
    modified_files: Set[str] = set()

    for rule, findings in findings_by_rule.items():
        for finding in findings:
            fix = _generate_fix(rule, finding)
            if fix:
                finding["extra"]["fix"] = fix
                filepath = finding["path"]
                try:
                    _modify_file(finding)
                    modified_files.add(filepath)
                except Exception as e:
                    print_error_exit(f"unable to modify file: {filepath}: {e}")
    num_modified = len(modified_files)
    print_msg(
        f"Successfully modified {num_modified} file{'s' if num_modified > 1 else ''}."
    )
