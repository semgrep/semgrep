"""
Parsers for yarn.lock versions 1 and 2/3
Version 1 parser based on
https://www.arahansen.com/the-ultimate-guide-to-yarn-lock-lockfiles/
https://classic.yarnpkg.com/lang/en/docs/yarn-lock/

Version 2/3 parser based on looking at examples on github, I could not find any documentation
Here are the Yarn 2/3 docs: https://yarnpkg.com/
"""
from pathlib import Path
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import TypedDict
from typing import TypeVar
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import extract_npm_lockfile_hash
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import line
from semdep.parsers.util import mark_line
from semdep.parsers.util import pair
from semdep.parsers.util import quoted
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyChild
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

A = TypeVar("A")


class DependencyDict(TypedDict):
    # Type def for the returned value from parsing dependency fields in yarn.lock
    version: str
    resolved: Union[str, None]
    checksum: Optional[str]
    children: List[Tuple[str, str]]


def create_dependency_dict(
    dependency_info: list[tuple[str, str]], children: list[tuple[str, str]]
) -> DependencyDict:
    raw_dict = {}
    for key, value in dependency_info:
        raw_dict[key] = value
    dependency_dict = DependencyDict(
        {
            "version": raw_dict.get("version", ""),
            "resolved": raw_dict.get("resolved", None),
            "checksum": raw_dict.get(
                "checksum", raw_dict.get("integrity", None)
            ),  # yarn 1 uses "integrity" instead of "checksum"
            "children": children,
        }
    )
    return dependency_dict


def dep_version_pair(dep: str, version: str) -> Tuple[str, str]:
    """
    Given a dependency and a version, return a tuple of the dependency and version
    """
    stripped_version = remove_npm_prefix(version)
    split_stripped_version = stripped_version.split("@")
    if len(split_stripped_version) > 1 and not (
        "ssh://" in split_stripped_version[0] or "https://" in split_stripped_version[0]
    ):  # Detect npm aliasing
        if (
            split_stripped_version[0] == ""
        ):  # Indicates that the package being aliased is scoped itself - use the next element for the package name instead
            if len(split_stripped_version) > 2:
                return (f"@{split_stripped_version[1]}", split_stripped_version[2])
            else:
                # Indicates that the package being aliased was aliased with no version constraint
                # Discard the version constraint since the "version" field will be populated in the lockfile anyway
                return (f"@{split_stripped_version[1]}", "")
        return (split_stripped_version[0], split_stripped_version[1])
    else:
        return (dep, stripped_version)


# The initial line of a yarn version 1 dependency, lists the constraints that lead to this package
# Examples:
# "@ampproject/remapping@^2.0.0":
# bad-lib@0.0.8:
# my-package-without-version-constraint:
# "filedep@file:../../correct/path/filedep":
# "bats@https://github.com/bats-core/bats-core#master":
part1 = regex('"?(@?[^@:]*)', flags=0, group=1)
part2 = regex('@?([^:,"]*(:?(?!\n)[^:,"]*)*)"?', flags=0, group=1)
source1 = pair(part1, part2)

# Examples:
# "@ampproject/remapping@^2.0.0", "@ampproject/remapping@^3.1.0"
# bad-lib@0.0.8, bad-lib@^0.0.4
multi_source1 = source1.sep_by(string(", "))

# A key value pair. These can be a name followed by a nested list, but the only data we care about is in outermost list
# This is why we produce None if the line is preceded by more than 2 spaces, or if it ends in a colon
# Examples:
#   version "2.1.1"
#   integrity sha512-Aolwjd7HSC2PyY0fDj/wA/EimQT4HfEnFYNp5s9CQlrdhyvWTtvZ5YzrUPu6R6/1jKiUlxu8bUhkdSnKHNAHMA==
#   dependencies:
key_value1 = string(" ").many() >> upto(" ").bind(
    lambda key: string(" ")
    >> upto("\n").bind(lambda value: success((key, value.strip('"'))))
)

dependencies1 = (
    string("\n  dependencies:\n    ")
    .optional()
    .bind(
        lambda title: key_value1.sep_by(string("\n")).map(
            lambda child_info: [
                dep_version_pair(dep.strip('"'), version) for dep, version in child_info
            ],
        )
        if title
        else success([])
    )
)

# A full spec of a dependency
# Examples:
# "@ampproject/remapping@^2.0.0":
#   version "2.1.1"
#   resolved "https://registry.npmjs.org/@ampproject/remapping/-/remapping-2.1.1.tgz"
#   integrity sha512-Aolwjd7HSC2PyY0fDj/wA/EimQT4HfEnFYNp5s9CQlrdhyvWTtvZ5YzrUPu6R6/1jKiUlxu8bUhkdSnKHNAHMA==
#   dependencies:
#     "@jridgewell/trace-mapping" "^0.3.0"
yarn_dep1 = mark_line(
    pair(
        multi_source1 << string(":\n"),
        key_value1.sep_by(string("\n")).bind(
            lambda dep_info: dependencies1.map(
                lambda deps: create_dependency_dict(
                    dependency_info=[x for x in dep_info],
                    children=deps,
                )
            )
            << (
                string("\n  ").optional()
                >> line.sep_by(string("\n  ")).map(lambda _: None)
            )
        ),
    )
)

YARN1_PREFIX = """\
# THIS IS AN AUTOGENERATED FILE. DO NOT EDIT THIS FILE DIRECTLY.
# yarn lockfile v1

"""

yarn1 = (
    string(YARN1_PREFIX)
    >> string("\n").optional()
    >> yarn_dep1.sep_by(string("\n\n"))
    << (string("\n\n") | string("\n")).optional()
)


# The yarn version 2/3 parser is set up equivalently, with slight differences in sub-parsers


def remove_npm_prefix(s: str) -> str:
    if s.startswith("npm:"):
        return s[4:]
    else:
        return s


# Examples:
# @ampproject/remapping@npm:^2.0.0
# @my-scope/my-first-package@my-scope/my-first-package#commit=0b824c650d3a03444dbcf2b27a5f3566f6e41358
# my-third-package@https://github.com/my-org/my-third-package#everything
# my-package@file:../../deps/my-local-package::locator=my-project%40workspace%3A.
# resolve@patch:resolve@^1.1.7#~builtin<compat/resolve>
source2 = pair(
    string("@").optional("") + upto("@", consume_other=True),
    # We remove the "npm:" prefix, because in a package.json, the version constraint will appear without it
    # e.g. "^1.0.0" in package.json becomes "npm:^1.0.0" in yarn.lock
    # However, prefix like "file:" *do* appear in package.json, so they aren't removed
    upto('"', ",").map(remove_npm_prefix),
)

# Examples:
# "@apidevtools/json-schema-ref-parser@npm:9.0.9"
# "@babel/generator@npm:^7.12.11, @babel/generator@npm:^7.12.5, @babel/generator@npm:^7.18.10"
multi_source2 = quoted(source2.sep_by(string(", ")))

# Examples:
#   version: 7.18.10
#   resolution: "@babel/generator@npm:7.18.10"
#   dependencies:
key_value2 = string(" ").many() >> upto(": ").bind(
    lambda key: string(": ")
    >> upto("\n").bind(lambda value: success((key, value.strip('"'))))
)


dependencies2 = (
    string("\n  dependencies:\n    ")
    .optional()
    .bind(
        lambda title: key_value2.sep_by(string("\n    ")).map(
            lambda child_info: [
                dep_version_pair(dep.strip('"'), version) for dep, version in child_info
            ],
        )
        if title
        else success([])
    )
)


# Examples:
# "@babel/generator@npm:^7.17.0, @babel/generator@npm:^7.7.2":
#   version: 7.17.0
#   resolution: "@babel/generator@npm:7.17.0"
#   dependencies:
#     "@babel/types": ^7.17.0
#     jsesc: ^2.5.1
#     source-map: ^0.5.0
#   checksum: 2987dbebb484727a227f1ce3db90810320986cfb3ffd23e6d1d87f75bbd8e7871b5bc44252822d4d5f048a2d872a5702b2a9bf7bab7e07f087d7f306f0ea6c0a
#   languageName: node
#   linkType: hard
yarn_dep2 = mark_line(
    pair(
        multi_source2 << string(":\n"),
        key_value2.sep_by(string("\n")).bind(
            lambda dep_info: dependencies2.map(
                lambda deps: create_dependency_dict(
                    dependency_info=[x for x in dep_info], children=deps
                )
            )
            << (
                string("\n  ").optional()
                >> line.sep_by(string("\n  ")).map(lambda _: None)
            )
        ),
    ),
)

YARN2_PREFIX = """\
# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!
"""
YARN2_METADATA_REGEX = """\

__metadata:
  version: \\d+
  cacheKey: \\d+(c\\d)?
"""
yarn2 = (
    string(YARN2_PREFIX)
    >> regex(YARN2_METADATA_REGEX).optional()
    >> string("\n").optional()
    >> yarn_dep2.sep_by(string("\n\n"))
    << (string("\n\n") | string("\n")).optional()
)


def get_manifest_deps(
    parsed_manifest: Optional[JSON],
) -> Optional[Set[Tuple[str, str]]]:
    """
    Extract a set of constraints from a package.json file
    """
    if not parsed_manifest:
        return None
    if not parsed_manifest:
        return None
    json = parsed_manifest.as_dict()
    deps = json.get("dependencies")
    dev_deps = json.get("devDependencies")
    all_deps = set()
    if deps:
        all_deps.update([(x[0], x[1].as_str()) for x in deps.as_dict().items()])
    if dev_deps:
        all_deps.update([(x[0], x[1].as_str()) for x in dev_deps.as_dict().items()])

    return all_deps


def remove_trailing_octothorpe(s: Optional[str]) -> Optional[str]:
    if s is None:
        return None
    else:
        return "#".join(s.split("#")[:-1]) if "#" in s else s


def parse_yarn(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    with open(lockfile_path) as f:
        lockfile_text = f.read()
    yarn_version = 1 if lockfile_text.startswith(YARN1_PREFIX) else 2
    parser = yarn1 if yarn_version == 1 else yarn2
    parser_name = (
        ScaParserName(out.PYarn1())
        if yarn_version == 1
        else ScaParserName(out.PYarn2())
    )
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, parser, parser_name),
        DependencyFileToParse(manifest_path, json_doc, ScaParserName(out.PJsondoc()))
        if manifest_path
        else None,
    )

    if not parsed_lockfile:
        return [], errors

    manifest_deps = get_manifest_deps(parsed_manifest)
    output = []
    dep_version_map = {}
    for _line_number, (sources, fields) in parsed_lockfile:
        if len(sources) < 1 or "version" not in fields:
            continue
        sources = [
            dep_version_pair(package, version_req) for package, version_req in sources
        ]
        for source in sources:
            dep_version_map[source] = fields["version"]
    for line_number, (sources, fields) in parsed_lockfile:
        if len(sources) < 1:
            continue
        if "version" not in fields:
            errors.append(
                DependencyParserError(
                    path=Fpath(str(lockfile_path)),
                    parser=parser_name,
                    reason="Missing version field",
                    line_number=line_number,
                )
            )
            continue
        sources = [
            dep_version_pair(package, version_req) for package, version_req in sources
        ]
        if yarn_version == 1:
            allowed_hashes = extract_npm_lockfile_hash(fields.get("checksum"))
        else:
            checksum = fields.get("checksum")
            allowed_hashes = {"sha512": [checksum]} if checksum else {}
        resolved_url = fields.get("resolved")

        raw_children = fields["children"]
        children = []
        for child in raw_children:
            try:
                children.append(
                    DependencyChild(
                        package=child[0],
                        version=dep_version_map[child],
                    )
                )
            except KeyError:
                errors.append(
                    DependencyParserError(
                        path=Fpath(str(lockfile_path)),
                        parser=parser_name,
                        reason=f"Child dependency version not found for child package {child[0]} of parent package {sources[0][0]}",
                        line=line_number,
                    )
                )
                continue

        output.append(
            FoundDependency(
                package=sources[0][0],
                version=fields["version"],
                ecosystem=Ecosystem(Npm()),
                allowed_hashes=allowed_hashes,
                resolved_url=remove_trailing_octothorpe(resolved_url),
                transitivity=transitivity(manifest_deps, sources),
                line_number=line_number,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
                children=children,
            )
        )
    return output, errors
