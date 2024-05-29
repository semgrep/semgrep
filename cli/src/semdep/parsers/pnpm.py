"""
Parsers for pnpm-lock.yaml files
Based on https://github.com/pnpm/spec/blob/master/lockfile/5.2.md
"""
import re
from pathlib import Path
from typing import List
from typing import Optional
from typing import Tuple

from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import PnpmLock
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName


def get_key_values(yaml: YamlTree[YamlMap], field: str) -> List[str]:
    try:
        map = yaml.value[field].value
        return [k.value for k in map.keys()] if map else []
    except KeyError:
        return []


def parse_direct_pre_6(yaml: YamlTree[YamlMap]) -> List[str]:
    return get_key_values(yaml, "specifiers")


def parse_package_key_pre_6(key: str) -> Optional[Tuple[str, str]]:
    # /package/version /@babel/helper-string-parser/7.19.4:
    match = re.compile(r"/(.+)/([^/]+)").match(key)
    return match.groups() if match else None  # type: ignore


def parse_direct_post_6(yaml: YamlTree[YamlMap]) -> List[str]:
    return get_key_values(yaml, "dependencies") + get_key_values(
        yaml, "devDependencies"
    )


def parse_package_key_post_6(key: str) -> Optional[Tuple[str, str]]:
    # /package@version or /@scope/package@version
    # starting / seems optional https://github.com/pnpm/pnpm/pull/7752/files
    match = re.compile(r"/?(.+?)@([^(@]+)").match(key)
    return match.groups() if match else None  # type: ignore


def parse_package_key_post_9(key: str) -> Optional[Tuple[str, str]]:
    # package@version or '@scope/package@version'
    match = re.compile(r"(.+?)@([^(@]+)").match(key)
    return match.groups() if match else None  # type: ignore


def parse_pnpm(
    lockfile_path: Path, _: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(
            lockfile_path,
            lambda text: parse_yaml_preserve_spans(
                text, str(lockfile_path), allow_null=True
            ),
            ScaParserName(PnpmLock()),
        ),
        None,
    )

    if not parsed_lockfile or not isinstance(parsed_lockfile.value, YamlMap):
        return [], errors
    try:
        lockfile_version = float(parsed_lockfile.value["lockfileVersion"].value)
    except KeyError:
        return [], errors

    if lockfile_version <= 5.4:
        parse_direct, parse_package_key = parse_direct_pre_6, parse_package_key_pre_6
    elif lockfile_version < 9.0:
        parse_direct, parse_package_key = parse_direct_post_6, parse_package_key_post_6
    else:
        parse_direct, parse_package_key = parse_direct_post_6, parse_package_key_post_9

    if "importers" in parsed_lockfile.value:
        direct_deps = {
            x
            for _, v in parsed_lockfile.value["importers"].value.items()
            for x in parse_direct(v)
        }
    else:
        direct_deps = set(parse_direct(parsed_lockfile))
    try:
        package_map = parsed_lockfile.value["packages"].value
        if not package_map:
            return [], errors
        all_deps: List[Tuple[int, Tuple[str, str]]] = []
        for key, map in package_map.items():
            line: int = key.span.start.line
            if map.value and "name" in map.value and "version" in map.value:
                all_deps.append(
                    (
                        line,
                        (map.value["name"].value, map.value["version"].value),
                    )
                )
            else:
                data = parse_package_key(key.value)
                if data:
                    # re does not have a way for us to refine the type of the match to what we know it is
                    all_deps.append((line, data))
                else:
                    errors.append(
                        DependencyParserError(
                            path=str(lockfile_path),
                            parser=ScaParserName(PnpmLock()),
                            reason=f"Could not parse package key {key.value}",
                            line=line,
                        )
                    )

    except KeyError:
        return [], errors
    output = []
    for line_number, (package_str, version_str) in all_deps:
        if not package_str or not version_str:
            continue
        output.append(
            FoundDependency(
                package=package_str,
                version=version_str,
                ecosystem=Ecosystem(Npm()),
                transitivity=transitivity(direct_deps, [package_str]),
                line_number=line_number,
                allowed_hashes={},
            )
        )
    return output, errors
