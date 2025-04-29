"""
Parsers for poetry.lock and pyproject.toml files
I could not find any comprehensive description of this format online, I just looked at examples
If you find any sort of spec, please link it here
Here's the docs for poetry: https://python-poetry.org/docs/
"""
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple
from typing import TypedDict

import tomli

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.parsy import any_char
from semdep.external.parsy import eof
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.parsers import preprocessors
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import mark_line
from semdep.parsers.util import new_lines
from semdep.parsers.util import pair
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyChild
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName


@dataclass
class ValueLineWrapper:
    # A wrapper for a line of a value in a key-value pair so we don't use an ugly tuple
    line_number: int
    value: str


class DependencyDict(TypedDict):
    # Type def for the returned value from safe_parse_lockfile_and_manifest() when using the
    # poetry parser
    name: ValueLineWrapper
    version: ValueLineWrapper
    description: ValueLineWrapper
    children: List[str]


# These use [until] instead of [upto] because [upto] only works on single characters
# and [upto] works on arbitrary parsers (this makes it slower though)
# We don't care about the contents of any list or object values right now

# Using ]\n allows us to avoid issues with closing brackets inside strings
# Examples:
# [foo, bar]
# [
#   foo,
#   bar
# ]
list_value = (
    string("[")
    >> any_char.until(string("]") << (string("\n") | eof)).result("")
    << string("]")
)

# Examples:
# {version = "*", optional = true, markers = "python_full_version <= \"3.11.0a6\" and extra == \"toml\""}
object_value = (
    string("{")
    >> any_char.until(string("}") << (string("\n") | eof)).result("")
    << string("}")
)

# Examples:
# "foo"
# "foo[bar]"
quoted_value = (
    string('"')
    >> any_char.until(string('"')).concat().map(lambda x: x.strip('"'))
    << string('"')
    << any_char.optional().until(string("\n") | eof)
)

# Examples:
# foo
plain_value = upto("\n")

# Multi-line string handling for both single and double triple quotes
multi_line_quoted_value = (
    (string("'''") | string('"""'))  # Match both triple single and triple double quotes
    >> any_char.until(
        (string("'''") | string('"""')) << string("\n").optional()
    ).result("")
    << (string("'''\n") | string('"""\n'))
)

# A value in a key-value pair.
value = multi_line_quoted_value | list_value | object_value | quoted_value | plain_value

key = regex(r'("[^"]*"|[^\s=]+)\s*=\s*', flags=0, group=1).map(lambda x: x.strip('"'))

# A key-value pair.
# Examples:
# foo = bar
# foo = [
#     bar, baz
# ]
key_value = pair(key, value)

key_value_list = mark_line(key_value).sep_by(new_lines)


def dict_to_dependency_dict(
    dep: Tuple[int, Dict[str, ValueLineWrapper]], deps: Dict[str, List[str]]
) -> Tuple[int, DependencyDict]:
    """
    Transforms a parsed poetry dependency and its children from a raw dict into a typed DependencyDict
    """
    return (
        dep[0],
        DependencyDict(
            {
                "name": dep[1]["name"],
                "version": dep[1]["version"],
                "description": dep[1]["description"],
                "children": deps["children"],
            }
        ),
    )


# A poetry dependency
# Example:
# [[package]]
# name = "factory-boy"
# version = "3.2.1"
# description = "A versatile test fixtures replacement based on thoughtbot's factory_bot for Ruby."
# category = "main"
# optional = false
# python-versions = ">=3.6"
poetry_dep = mark_line(
    string("[[package]]\n")
    >> key_value_list.map(
        lambda x: {
            key_val[0]: ValueLineWrapper(line_number, key_val[1])
            for line_number, key_val in x
        }
    )
)

# Parses dependency relationships from poetry.lock for a single dependency
# Example:
# [package.dependencies]
# aiohappyeyeballs = ">=2.3.0"
# aiosignal = ">=1.1.2"
# async-timeout = {version = ">=4.0,<6.0", markers = "python_version < \"3.11\""}
# attrs = ">=17.3.0"
# frozenlist = ">=1.1.1"
# multidict = ">=4.5,<7.0"
# yarl = ">=1.12.0,<2.0"
#
# will be transformed into a raw dict:
# {
#     "children": [
#         "aiohappyeyeballs",
#         "aiosignal",
#         ...
#     ]
# }
#
# Accepts a parsed poetry dependency and adds its parsed children (returned by the dependencies_parser)
# in a typed format (handled by dict_to_dependency_dict)
dependencies_parser = new_lines.many() >> string(
    "[package.dependencies]\n"
).optional().bind(
    lambda title: key_value_list.map(
        lambda child_info: {"children": [dep[0] for _, dep in child_info]}
    )
    if title
    else success({"children": []})
)

# Enriches the output of the poetry_dep parser with dependency relationships for
# the single dependency being parsed
poetry_dep_ptt_parser = poetry_dep.bind(
    lambda poetry_dep_info: dependencies_parser.map(
        lambda deps: dict_to_dependency_dict(poetry_dep_info, deps)
    )
)


# Poetry Source which we ignore
# Example:
# [[tool.poetry.source]]
# name = "semgrep"
# url = "https://artifact.semgrep.com/"
# secondary = False
poetry_source_extra = (
    string("[[")
    >> upto("]")
    << string("]]\n")
    << new_lines.optional()
    >> key_value_list
).map(lambda _: None)

# Extra data from a dependency, which we just treat as standalone data and ignore
# The at_least(1) is to handle empty tables with no extra newlines. This was easier than overhauling everything to support that
# Example:
# [package.extras]
# dev = ["coverage", "django", "flake8", "isort", "pillow", "sqlalchemy", "mongoengine", "wheel (>=0.32.0)", "tox", "zest.releaser"]
# doc = ["sphinx", "sphinx-rtd-theme", "sphinxcontrib-spelling"]
#
# [package.dependencies]
# [package.dependencies.typing_extensions]
# python = "<3.10"
# version = ">=4.0"
poetry_dep_extra = (string("[") >> upto("]") << string("]\n")).at_least(
    1
) >> key_value_list.map(lambda _: None)

# A whole poetry file
poetry = (
    string("\n").many()
    >> (
        poetry_dep_ptt_parser | poetry_dep_extra | (string("package = []").result(None))
    )
    .sep_by(new_lines.optional())
    .map(lambda xs: [x for x in xs if x])
    << new_lines.optional()
)


def parse_pyproject_toml(
    raw_manifest: str,
) -> set[str]:
    parsed_manifest = tomli.loads(raw_manifest)
    manifest_deps: set[str] = set(
        parsed_manifest.get("tool", {}).get("poetry", {}).get("dependencies", {}).keys()
    )
    return manifest_deps


def parse_poetry(
    lockfile_path: Path,
    manifest_path: Optional[Path],
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, _, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(
            lockfile_path,
            poetry,
            ScaParserName(out.PPoetryLock()),
            preprocessors.CommentRemover(),
        ),
        None,
    )
    if manifest_path:
        try:
            raw_manifest = manifest_path.read_text()
            manifest_deps = parse_pyproject_toml(raw_manifest)
        except Exception as e:
            errors.append(
                DependencyParserError(
                    path=Fpath(str(manifest_path)),
                    parser=ScaParserName(
                        out.PPyprojectToml()
                    ),  # There is actually no longer a custom parser for this since pyproject.toml is now parsed by a TOML parser (tomli)
                    reason=f"Failed to parse [bold]{manifest_path}[/bold]: {str(e)}",
                )
            )
            manifest_deps = set()
    else:
        manifest_deps = set()

    if not parsed_lockfile:
        return [], errors

    # According to PEP 426: pypi distributions are case insensitive and consider hyphens and underscores to be equivalent
    sanitized_manifest_deps = {dep.lower().replace("-", "_") for dep in manifest_deps}

    output = []
    dep_version_map = {}
    for _line_number, dep in parsed_lockfile:
        if "name" not in dep or "version" not in dep:
            continue
        dep_version_map[dep["name"].value.lower().replace("_", "-")] = dep[
            "version"
        ].value
    for _line_number, dep in parsed_lockfile:
        if "name" not in dep or "version" not in dep:
            continue
        output.append(
            FoundDependency(
                package=dep["name"].value.lower(),
                version=dep["version"].value,
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=transitivity(
                    sanitized_manifest_deps,
                    [dep["name"].value.lower().replace("-", "_")],
                ),
                line_number=dep["name"].line_number,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
                children=[
                    DependencyChild(
                        package=child.lower().replace(
                            "_", "-"
                        ),  # See earlier comment about PEP 426 (dependencies are currently stored with hyphens)
                        version=dep_version_map[child.lower().replace("_", "-")],
                    )
                    for child in dep.get("children", [])
                    if child.lower().replace("_", "-") in dep_version_map
                ],
            )
        )
    return output, errors
