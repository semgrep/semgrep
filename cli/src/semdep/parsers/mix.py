from __future__ import annotations

from pathlib import Path

from semdep.external.parsy import any_char
from semdep.external.parsy import Parser
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.parsers.util import colon
from semdep.parsers.util import comma
from semdep.parsers.util import consume_line
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import lbrace
from semdep.parsers.util import lbrack
from semdep.parsers.util import line
from semdep.parsers.util import mark_line
from semdep.parsers.util import new_lines
from semdep.parsers.util import quoted_str
from semdep.parsers.util import rbrace
from semdep.parsers.util import rbrack
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semdep.parsers.util import whitespace
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Hex
from semgrep.semgrep_interfaces.semgrep_output_v1 import MixLock
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName

# :hex,
atom = string(":") >> upto(",", consume_other=False)

# Lockfile
scm = atom
package_block = atom

# "2.7.0"
version_block = quoted_str

# [:mix], [], [:mix, :rebar3]
any_array = lbrack >> any_char.until(rbrack, consume_other=True)

# {:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]}
inner_dependency_block = (
    lbrace >> atom >> comma >> quoted_str >> comma >> any_array >> rbrace
)

# {:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]}, {:octo_fetch, "~> 0.3", [hex: :octo_fetch, repo: "hexpm", optional: false]}
many_independency_blocks = inner_dependency_block.sep_by(comma)

git_ref = string("ref") >> colon >> quoted_str

git_branch = string("branch") >> colon >> quoted_str

comment = regex(r" *#") >> line


def git_tag(package: str) -> Parser[tuple[int, tuple[str, str]]]:
    return (
        string("tag")
        >> colon
        >> version_block.bind(lambda version: mark_line(success((package, version))))
    )


#   {
#     :hex,
#     :testing,
#     "1.0.1",
#     "1234bb4db5b32fc0f8aa5c4a2040348b4aa36687100fb8837b850e90cf60e06",
#     [:mix],
#     [],
#     "hexpm",
#     "98767a5d1c6c3e3d20497b03293be7f83b46f89a6f3987cc1f9262d299f1eaa7"
#   }
def package_entry_hex_value_block(package: str) -> Parser[tuple[int, tuple[str, str]]]:
    return (
        whitespace
        >> lbrace
        >> atom
        >> comma
        >> atom
        >> comma
        >> version_block.bind(lambda version: mark_line(success((package, version))))
        << comma
        << quoted_str
        << comma
        << any_array
        << comma
        << lbrack
        << many_independency_blocks
        << rbrack
        << comma
        << quoted_str
        << (comma << quoted_str).optional()
        << rbrace
        << comma.optional()
    )


# {:git, "https://github.com/emqx/grpc-erl.git", "31370f25643666c4be43310d62ef749ca1fc20e2", [tag: "0.6.12"]},
def package_entry_git_value_block(
    package: str,
) -> Parser[tuple[int, tuple[str, str]] | str | None]:
    return (
        whitespace
        >> lbrace
        >> atom
        >> comma
        >> quoted_str
        >> comma
        >> quoted_str
        >> comma
        >> lbrack
        >> (git_tag(package) | git_ref | git_branch).optional()
        << rbrack
        << rbrace
        << comma.optional()
    )


# "castore": {:hex, :castore, "1.0.5", "9eeebb394cc9a0f3ae56b813459f990abb0a3dedee1be6b27fdb50301930502f", [: mix], [], "hexpm", "8d7c597c3e4a64c395980882d4bca3cebb8d74197c590dc272cfd3b6a6310578"},
# "grpc": {:git, "https://github.com/emqx/grpc-erl.git", "31370f25643666c4be43310d62ef749ca1fc20e2", [tag: "0.6.12"]},
package_key_value_block = whitespace >> quoted_str.bind(
    lambda package: colon
    >> (
        package_entry_hex_value_block(package)
        | package_entry_git_value_block(package)
        | consume_line
    )
)

many_package_blocks = package_key_value_block.sep_by(whitespace)

lockfile_parser = (
    whitespace >> string("%") >> lbrace >> many_package_blocks << whitespace << rbrace
)

# Manifest

# {:ehttpc, github: "emqx/ehttpc", tag: "0.4.13", override: true}
manifest_package = (
    whitespace
    >> lbrace
    >> atom
    << any_char.until(rbrace << string(",").optional(), consume_other=True)
)

# {:ehttpc, github: "emqx/ehttpc", tag: "0.4.13", override: true},
# {:gproc, github: "emqx/gproc", tag: "0.9.0.1", override: true},
# {:rocksdb, github: "emqx/erlang-rocksdb", tag: "1.8.0-emqx-2", override: true},
many_manifest_packages = whitespace >> (
    comment | (mark_line(manifest_package) << comment.optional())
).sep_by(new_lines)

# defp deps do
manifest_deps_declaration = regex("defp? +deps.*?do")

# defp deps do
#     [
#       {:ehttpc, github: "emqx/ehttpc", tag: "0.4.13", override: true},
#       {:gproc, github: "emqx/gproc", tag: "0.9.0.1", override: true},
#       {:rocksdb, github: "emqx/erlang-rocksdb", tag: "1.8.0-emqx-2", override: true},
#       {:grpc, github: "emqx/grpc-erl", tag: "0.6.12", override: true},
#       {:ecpool, github: "emqx/ecpool", tag: "0.5.7", override: true},
#       {:pbkdf2, github: "emqx/erlang-pbkdf2", tag: "2.0.4", override: true},
#       {:typerefl, github: "ieQu1/typerefl", tag: "0.9.1", override: true}
#     ]
# end
manifest_deps = (
    whitespace
    >> manifest_deps_declaration
    >> whitespace
    >> lbrack
    >> many_manifest_packages
    << whitespace
    << rbrack
    << string("end")
    << whitespace
)

inline_deps = string("deps") >> colon

inline_manifest_deps = (
    whitespace
    >> inline_deps
    >> whitespace
    >> lbrack
    >> many_manifest_packages
    << any_char.many()
)

manifest_parser = (
    (any_char.until(manifest_deps_declaration) >> manifest_deps)
    | (any_char.until(inline_deps) >> inline_manifest_deps)
) << any_char.many()


def _parse_manifest_deps(manifest: list[tuple[int, str]]) -> set[str]:
    result = set()
    for _line_number, package in manifest:
        result.add(package.lower())

    return result


def _build_found_dependencies(
    direct_deps: set[str], lockfile_deps: list[tuple[int, tuple[str, str]] | None]
) -> list[FoundDependency]:
    result = []
    for dep in lockfile_deps:
        if dep is None:
            continue
        line_number, (package, version) = dep
        result.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Hex()),
                allowed_hashes={},
                transitivity=transitivity(direct_deps, [package]),
                line_number=line_number,
            )
        )

    return result


def parse_mix(
    lockfile_path: Path, manifest_path: Path | None
) -> tuple[list[FoundDependency], list[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, lockfile_parser, ScaParserName(MixLock())),
        (
            DependencyFileToParse(
                manifest_path, manifest_parser, ScaParserName(MixLock())
            )
            if manifest_path
            else None
        ),
    )

    if not parsed_lockfile or not parsed_manifest:
        return [], errors

    direct_deps = _parse_manifest_deps(
        [x for x in parsed_manifest if isinstance(x, tuple)]
    )
    found_deps = _build_found_dependencies(
        direct_deps, [x for x in parsed_lockfile if isinstance(x, tuple)]
    )

    return found_deps, errors
