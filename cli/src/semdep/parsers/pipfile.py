"""
Parser for pipfile.lock files
Based on https://pipenv.pypa.io/en/latest/basics/
"""
from collections import defaultdict
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

from semdep.external.parsy import regex
from semdep.parsers import preprocessors
from semdep.parsers.poetry import key_value
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import json_doc
from semdep.parsers.util import pair
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Jsondoc
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pipfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


manifest_block = pair(
    regex(r"\[(.*)\]\n+", flags=0, group=1), key_value.sep_by(regex(r"\n+"))
)

manifest = (
    manifest_block.map(
        lambda block: None
        if block[0] not in ["packages", "dev-packages"]
        else {x[0] for x in block[1]}
    )
    .sep_by(regex(r"\n+").at_least(1))
    .map(lambda sets: {x for s in sets if s for x in s})
    << regex(r"\n+").optional()
)


def parse_pipfile(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:

    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(Jsondoc())),
        DependencyFileToParse(
            manifest_path,
            manifest,
            ScaParserName(Pipfile()),
            preprocessors.CommentRemover(),
        )
        if manifest_path
        else None,
    )

    if not parsed_lockfile:
        return [], errors

    deps = parsed_lockfile.as_dict()["default"].as_dict()

    # According to PEP 426: pypi distributions are case insensitive and consider hyphens and underscores to be equivalent
    sanitized_manifest_deps = (
        {dep.lower().replace("-", "_") for dep in parsed_manifest}
        if parsed_manifest
        else parsed_manifest
    )

    def extract_pipfile_hashes(
        hashes: List[str],
    ) -> Dict[str, List[str]]:
        output = defaultdict(list)
        for h in hashes:
            parts = h.split(":")
            if len(parts) < 2:
                continue
            algorithm = parts[0]
            rest = h[len(algorithm) + 1 :]  # pipfile is already in base16
            output[algorithm].append(rest.lower())
        return output

    output = []
    for package, dep_json in deps.items():
        fields = dep_json.as_dict()
        if "version" not in fields:
            logger.info(f"no version for dependency: {package}")
            continue
        version = fields["version"].as_str()
        version = version.replace("==", "")
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Pypi()),
                resolved_url=None,
                allowed_hashes=extract_pipfile_hashes(
                    [h.as_str() for h in fields["hashes"].as_list()]
                )
                if "hashes" in fields
                else {},
                transitivity=transitivity(
                    sanitized_manifest_deps, [package.lower().replace("-", "_")]
                ),
                line_number=dep_json.line_number,
            )
        )
    return output, errors
