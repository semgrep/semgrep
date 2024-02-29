"""
Parsers for poetry.lock and pyproject.toml files
I could not find any comprehensive description of this format online, I just looked at examples
If you find any sort of spec, please link it here
Here's the docs for poetry: https://python-poetry.org/docs/
"""
from dataclasses import dataclass
from pathlib import Path
from typing import List
from typing import Optional
from typing import Tuple

from semdep.external.parsy import any_char
from semdep.external.parsy import eof
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers import preprocessors
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import mark_line
from semdep.parsers.util import new_lines
from semdep.parsers.util import pair
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import PoetryLock
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import PyprojectToml
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName


@dataclass
class ValueLineWrapper:
    # A wrapper for a line of a value in a key-value pair so we don't use an ugly tuple
    line_number: int
    value: str


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
    >> any_char.until(string('"\n')).concat().map(lambda x: x.strip('"'))
    << string('"')
)

# Examples:
# foo
plain_value = upto("\n")

# A value in a key-value pair.
value = list_value | object_value | quoted_value | plain_value

key = regex(r'("[^"]*"|[^\s=]+)\s*=\s*', flags=0, group=1).map(lambda x: x.strip('"'))

# A key-value pair.
# Examples:
# foo = bar

# foo = [
#     bar, baz
# ]
key_value = pair(key, value)

key_value_list = mark_line(key_value).sep_by(new_lines)

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
    >> (poetry_dep | poetry_dep_extra | (string("package = []").result(None)))
    .sep_by(new_lines.optional())
    .map(lambda xs: [x for x in xs if x])
    << new_lines.optional()
)


# Direct dependencies listed in a pyproject.toml file
# Example:
# [tool.poetry.dependencies]
# python = "^3.10"
# faker = "^13.11.0"
manifest_deps = (
    string("[tool.poetry.dependencies]\n")
    << new_lines.optional()
    >> key_value.map(lambda x: x[0]).sep_by(new_lines)
)

manifest_sections_extra = (
    (string("[") >> upto("]") << string("]\n")).at_least(1)
    >> new_lines.optional()
    >> key_value_list.map(lambda _: None)
)

# A whole pyproject.toml file. We only about parsing the manifest_deps
manifest = (
    string("\n").many()
    >> (manifest_deps | manifest_sections_extra | poetry_source_extra)
    .sep_by(new_lines.optional())
    .map(lambda xs: {y for x in xs if x for y in x})
    << new_lines.optional()
)


def parse_poetry(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(
            lockfile_path,
            poetry,
            ScaParserName(PoetryLock()),
            preprocessors.CommentRemover(),
        ),
        DependencyFileToParse(
            manifest_path,
            manifest,
            ScaParserName(PyprojectToml()),
            preprocessors.CommentRemover(),
        )
        if manifest_path
        else None,
    )
    if not parsed_lockfile:
        return [], errors

    # According to PEP 426: pypi distributions are case insensitive and consider hyphens and underscores to be equivalent
    sanitized_manifest_deps = (
        {
            dep.lower().replace("-", "_")
            for dep in (parsed_manifest if parsed_manifest else set())
        }
        if parsed_manifest
        else parsed_manifest
    )

    output = []
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
            )
        )
    return output, errors
