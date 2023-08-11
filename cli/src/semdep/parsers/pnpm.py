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


def parse_direct_post_6(yaml: YamlTree[YamlMap]) -> List[str]:
    return get_key_values(yaml, "dependencies") + get_key_values(
        yaml, "devDependencies"
    )


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
        parse_direct = parse_direct_pre_6
    else:
        parse_direct = parse_direct_post_6

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
        all_deps: List[tuple[int, tuple[str, str]]] = []
        for key, map in package_map.items():
            if map.value and "name" in map.value and "version" in map.value:
                all_deps.append(
                    (
                        key.span.start.line,
                        (map.value["name"].value, map.value["version"].value),
                    )
                )
            else:
                match = re.compile(r"/(.+)/([^/]+)").match(key.value)
                if match:
                    # re does not have a way for us to refine the type of the match to what we know it is
                    all_deps.append((key.span.start.line, match.groups()))  # type: ignore

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
