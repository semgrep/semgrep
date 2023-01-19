"""
Parsers for poetry.lock and pyproject.toml files, and a function to produce FoundDependency objects from them
"""
from pathlib import Path
from typing import List
from typing import Optional

from parsy import string
from parsy import success

from semdep.parsers.util import mark_line
from semdep.parsers.util import not_any
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi


# A value in a key-value pair.
# Examples:
# bar

# [
#     bar, baz
# ]
# If the value is a list we don't bother reading the content, because they are not useful for our SCA
value = not_any(["\n", "["]).map(lambda x: x.strip('"')) | (
    string("[") >> not_any(["]"]) << string("]")
).map(lambda _: "")


# A key-value pair.
# Examples:
# foo = bar

# foo = [
#     bar, baz
# ]
key_value = not_any([" ", "\n"]).bind(
    lambda key: string(" = ") >> value.bind(lambda value: success((key, value)))
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
    string("[[package]]\n") >> key_value.sep_by(string("\n")).map(lambda x: dict(x))
)

# Extra data from a dependency, which we just treat as standalone data and ignore
# Example:
# [package.extras]
# dev = ["coverage", "django", "flake8", "isort", "pillow", "sqlalchemy", "mongoengine", "wheel (>=0.32.0)", "tox", "zest.releaser"]
# doc = ["sphinx", "sphinx-rtd-theme", "sphinxcontrib-spelling"]
poetry_dep_extra = (string("[") >> not_any(["]"]) << string("]\n")) >> key_value.sep_by(
    string("\n")
).map(lambda _: None)

# A whole poetry file
poetry = (poetry_dep | poetry_dep_extra).sep_by(string("\n\n")).map(
    lambda xs: [x for x in xs if x]
) << string("\n").optional()


# Direct dependencies listed in a pyproject.toml file
# Example:
# [tool.poetry.dependencies]
# python = "^3.10"
# faker = "^13.11.0"
manifest_deps = string("[tool.poetry.dependencies]\n") >> key_value.map(
    lambda x: x[0]
).sep_by(string("\n"))

# A whole pyproject.toml file. We only about parsing the manifest_deps
manifest = (manifest_deps | poetry_dep_extra).sep_by(
    string("\n").times(min=1, max=float("inf"))
).map(lambda xs: {y for x in xs if x for y in x}) << string("\n").optional()


def parse_poetry(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    deps = safe_path_parse(lockfile_path, poetry)
    if not deps:
        return []
    manifest_deps = safe_path_parse(manifest_path, manifest)
    output = []
    for line_number, dep in deps:
        if "name" not in dep or "version" not in dep:
            continue
        output.append(
            FoundDependency(
                package=dep["name"],
                version=dep["version"],
                ecosystem=Ecosystem(Pypi()),
                allowed_hashes={},
                transitivity=transitivity(manifest_deps, [dep["name"]]),
                line_number=line_number,
            )
        )
    return output
