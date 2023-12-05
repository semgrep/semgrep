"""
Parsers for pubspec.lock files
"""
from pathlib import Path
from typing import List
from typing import Optional
from typing import Tuple

from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import YamlMap
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pub
from semgrep.semgrep_interfaces.semgrep_output_v1 import PubspecLock
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown


def parse_pubspec_lock(
    lockfile_path: Path, _: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, _, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(
            lockfile_path,
            lambda text: parse_yaml_preserve_spans(
                text, str(lockfile_path), allow_null=True
            ),
            ScaParserName(PubspecLock()),
        ),
        None,
    )

    if not parsed_lockfile or not isinstance(parsed_lockfile.value, YamlMap):
        return [], errors

    output = []
    try:
        package_map = parsed_lockfile.value["packages"].value
        if not package_map:
            return [], errors

        for key, map in package_map.items():
            stated_transitivity = map.value["dependency"].value
            if stated_transitivity == "transitive":
                transitivity = Transitivity(Transitive())
            elif "direct" in stated_transitivity:
                transitivity = Transitivity(Direct())
            else:
                transitivity = Transitivity(Unknown())

            output.append(
                FoundDependency(
                    package=key.value,
                    version=map.value["version"].value,
                    ecosystem=Ecosystem(Pub()),
                    transitivity=transitivity,
                    line_number=key.span.start.line,
                    allowed_hashes={},
                )
            )
    except KeyError:
        return [], errors

    return output, errors
