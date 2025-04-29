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

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.parsy import any_char
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers import preprocessors
from semdep.parsers.poetry import key
from semdep.parsers.poetry import list_value
from semdep.parsers.poetry import multi_line_quoted_value
from semdep.parsers.poetry import object_value
from semdep.parsers.poetry import plain_value
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import json_doc
from semdep.parsers.util import mark_line
from semdep.parsers.util import pair
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

quoted_value = (
    string('"')
    >> any_char.until(string('"\n')).concat().map(lambda x: x.strip('"'))
    << string('"')
)

value = multi_line_quoted_value | list_value | object_value | quoted_value | plain_value
key_value = pair(key, value)


manifest_block = pair(
    regex(r"\[(.*)\]\n", flags=0, group=1), key_value.sep_by(regex(r"\n+"))
)

new_lines = regex("\n+")
key_value_list = mark_line(key_value).sep_by(new_lines)

manifest_deps = (
    (string("[packages]") | string("[dev-packages]"))
    << new_lines.optional()
    >> key_value.map(lambda x: x[0]).sep_by(new_lines)
)

manifest_sections_extra = (
    (
        string("[") >> upto("]") << string("]\n")
        | (string("[[") >> upto("]]") << string("]]\n"))
    ).at_least(1)
    >> new_lines.optional()
    >> key_value_list.map(lambda _: None)
)

manifest = (
    string("\n").many()
    >> (manifest_deps | manifest_sections_extra)
    .sep_by(new_lines.optional())
    .map(lambda sets: {x for s in sets if s for x in s})
    << new_lines.optional()
)


def parse_pipfile(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(out.PJsondoc())),
        DependencyFileToParse(
            manifest_path,
            manifest,
            ScaParserName(out.PPipfile()),
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
                package=package.lower(),
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
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
    return output, errors
