from typing import List
from typing import Tuple

import semgrep.ocaml as ocaml
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.util import unit_str
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def _match_to_edit(match: RuleMatch, fix: str) -> out.Edit:
    return out.Edit(
        out.Fpath(str(match.path)),
        match.start.offset,
        match.end.offset,
        fix,
    )


# If dryrun is true:
# - Apply the autofixes in the given matches.
# - Report the number of modified files.
# If dryrun is false:
# - Mutate the matches to add the `fixed_lines` property.
def apply_fixes(rule_matches_by_rule: RuleMatchMap, dryrun: bool = False) -> None:
    # This is implemented in OCaml and called via RPC. There are two
    # complications with that:
    # 1. We would like to inform the user of the number of modified files. We do
    #    that by returning the number of modified files from OCaml and printing
    #    it here.
    # 2. We would like to mutate the matches to add a `fixed_lines` property. To
    #    do that, we make a list of (rulematch, edit) pairs. We pass the edits
    #    to OCaml. Then we get a list of fixed_lines along with an index for the
    #    associated match. We use that index to mutate the appropriate fix.
    matches: List[Tuple[RuleMatch, out.Edit]] = []
    for _, rule_matches in rule_matches_by_rule.items():
        for match in rule_matches:
            fix = match.fix
            if fix is not None:
                matches.append(
                    (
                        match,
                        _match_to_edit(match, fix),
                    )
                )
    edits = [edit for (match, edit) in matches]
    rpc_args = out.ApplyFixesParams(dryrun, edits)
    application_result = ocaml.apply_fixes(rpc_args)
    # Put this case first because otherwise mypy requires a type annotation on
    # the empty list
    if application_result is not None:
        modified_files, fixed_lines = application_result.value
    else:
        logger.error(f"Fix application over RPC was unsuccessful")
        modified_files = 0
        fixed_lines = []
    # For each fixed_lines entry, use the index to look up the appropriate match
    # to mutate.
    for i, lines in fixed_lines:
        match, _ = matches[i]
        match.extra["fixed_lines"] = lines  # Monkey patch in fixed lines
    if not dryrun:
        if modified_files:
            logger.info(f"successfully modified {unit_str(modified_files, 'file')}.")
        else:
            logger.info(f"no files modified.")
