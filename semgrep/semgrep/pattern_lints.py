import json
from typing import Any
from typing import Dict

from glom import glom  # type: ignore
from glom import PathAccessError

from semgrep.dump_ast import parsed_ast


def patterns_are_equivalent(language: str, patt1: str, patt2: str) -> bool:
    patt1_json = json.loads(
        parsed_ast(language=language, to_json=True, pattern=patt1, targets_str=[])
    )
    patt2_json = json.loads(
        parsed_ast(language=language, to_json=True, pattern=patt2, targets_str=[])
    )

    if patt1_json == patt2_json:
        return True

    for s1, s2 in zip(patt1_json.get("Ss"), patt2_json.get("Ss")):
        if s1 == s2:
            continue

        statments = [s1, s2]
        ret = [s for s in statments if s.keys() == {"Return"}]
        expr = [s for s in statments if s.keys() == {"ExprStmt"}]
        if ret and expr:
            if assignment_matches_return(expr=expr[0], ret=ret[0]):
                continue
        return False
    return True


def assignment_matches_return(expr: Dict[str, Any], ret: Dict[str, Any]) -> bool:
    try:
        expr_rhs = glom(expr, "ExprStmt.Assign.2")
        return_rhs = glom(ret, "Return.1.some")
        return expr_rhs == return_rhs  # type: ignore
    except PathAccessError:
        return False
