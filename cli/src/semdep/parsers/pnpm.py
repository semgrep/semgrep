"""
Parsers for pnpm-lock.yaml files
Based on https://github.com/pnpm/spec/blob/master/lockfile/5.2.md
"""
import re
from pathlib import Path
from typing import List
from typing import Optional

from semdep.parsers.util import ParserName
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm


def parse_direct_pre_6(yaml: YamlTree) -> List[str]:
    try:
        return [k.value for k in yaml.value["specifiers"].value.keys()]
    except KeyError:
        return []


def parse_direct_post_6(yaml: YamlTree) -> List[str]:
    try:
        deps = [k.value for k in yaml.value["dependencies"].value.keys()]
    except KeyError:
        deps = []
    try:
        devDeps = [k.value for k in yaml.value["devDependencies"].value.keys()]
    except KeyError:
        devDeps = []
    return deps + devDeps


def parse_pnpm(lockfile_path: Path, _: Optional[Path]) -> List[FoundDependency]:
    yaml: Optional[YamlTree] = safe_path_parse(
        lockfile_path, parse_yaml_preserve_spans, ParserName.pnpm_lock
    )
    if not yaml or not isinstance(yaml.value, YamlMap):
        return []
    try:
        lockfile_version = float(yaml.value["lockfileVersion"].value)
    except KeyError:
        return []
    if lockfile_version <= 5.4:
        parse_direct = parse_direct_pre_6
    else:
        parse_direct = parse_direct_post_6

    if "importers" in yaml.value:
        direct_deps = {
            x for _, v in yaml.value["importers"].value.items() for x in parse_direct(v)
        }
    else:
        direct_deps = set(parse_direct(yaml))
    try:
        all_deps = [
            (k.span.start.line, match.groups())
            for k in yaml.value["packages"].value.keys()
            if (match := re.compile(r"/(.+)/([^/]+)").match(k.value))
        ]
    except KeyError:
        return []
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
    return output
