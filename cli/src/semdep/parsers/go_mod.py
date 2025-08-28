#
# Copyright (c) 2023-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
"""
Parser for go.mod files
Based on https://go.dev/ref/mod#go-mod-file
"""
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple
from typing import TypeVar

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.parsy import alt
from semdep.external.parsy import Parser
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import mark_line
from semdep.parsers.util import pair
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive

A = TypeVar("A")
B = TypeVar("B")


consume_line = regex(r"[^\n)]*").result(None)

comment = regex(r" *//([^\n]*)", flags=0, group=1)


def multi_spec(spec: "Parser[A]") -> "Parser[List[Tuple[A,Optional[str]]]]":
    return (
        regex(r"\s*\(\s+")
        >> (pair(spec, comment.optional(None)) << regex(r"\s+")).many()
        << string(")")
    ) | (regex(r"[ \t]*") >> pair(spec, comment.optional()).map(lambda x: [x]))


def make_directive(
    dir: "Parser[A]", spec: "Parser[B]"
) -> "Parser[Tuple[A,List[Tuple[B,Optional[str]]]]]":
    return pair(dir, multi_spec(spec))


dep_spec = regex(r"([^ \n]+) v([^ \n]+)", flags=0, group=(1, 2)) | comment.result(None)

# reference: https://go.dev/ref/mod#go-mod-file-grammar
# EBNF copied here:
# GoMod = { Directive } .
# Directive = ModuleDirective |
#             GoDirective |
#             ToolDirective |
#             IgnoreDirective |
#             RequireDirective |
#             ExcludeDirective |
#             ReplaceDirective |
#             RetractDirective .
# note that this EBNF seems to be missing the 'toolchain' and 'godebug' directives,
# which are described in the docs below the grammar section
specs: Dict[str, "Parser[Optional[Tuple[str,...]]]"] = {
    "module": comment.result(None) | consume_line,
    "go": comment.result(None) | consume_line,
    "tool": comment.result(None) | consume_line,
    "godebug": comment.result(None) | consume_line,
    "ignore": comment.result(None) | consume_line,
    "toolchain": comment.result(None) | consume_line,
    "require": dep_spec,
    "exclude": dep_spec,
    "replace": comment.result(None) | consume_line,
    "retract": comment.result(None) | consume_line,
}

directive = alt(
    *(make_directive(string(dir), mark_line(spec)) for dir, spec in specs.items())
)

go_mod = (
    (comment.optional() >> string("\n")).many()
    >> directive.sep_by((comment.optional() >> string("\n")).at_least(1))
    << (comment.optional() >> string("\n")).many()
)


def parse_go_mod(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, go_mod, ScaParserName(out.PGoMod())), None
    )
    if not parsed_lockfile:
        return [], errors
    exclude = set()
    output = []
    for dir, data in parsed_lockfile:
        if dir == "exclude":
            for (_, dep), _ in data:
                if dep:
                    package, version = dep
                    exclude.add((package, version))
        if dir == "require":
            for (line_number, dep), comment in data:
                if dep:
                    package, version = dep
                    output.append(
                        FoundDependency(
                            package=package,
                            version=version,
                            ecosystem=Ecosystem(Gomod()),
                            allowed_hashes={},
                            transitivity=out.DependencyKind(
                                Transitive() if comment == " indirect" else Direct()
                            ),
                            line_number=line_number,
                            resolved_url=package,  # Go package names are URLs
                            lockfile_path=Fpath(str(lockfile_path)),
                            manifest_path=Fpath(str(manifest_path))
                            if manifest_path
                            else None,
                        )
                    )
    return [d for d in output if (d.package, d.version) not in exclude], errors
