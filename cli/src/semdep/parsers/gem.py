"""
Parser for Gemfile.lock files
Based on https://stackoverflow.com/questions/7517524/understanding-the-gemfile-lock-file
"""
from pathlib import Path
from typing import List
from typing import Optional

from semdep.external.parsy import any_char
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.parsers.util import consume_line
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gem


# Examples:
# (1.25.0)
version = string("(") >> upto(")", consume_other=True)

# Examples:
#     rubocop (1.25.0)
package = string("    ") >> upto(" ", consume_other=True).bind(
    lambda package: version.bind(lambda version: success((package, version)))
)

# Examples:
#   faker!
#   minitest (= 5.15.0)
#   simplecov (= 0.17.1, < 0.18)
manifest_package = string("  ") >> upto(" ", "!").bind(
    lambda package: any_char.bind(
        lambda next: success(package) if next == "!" else version >> success(package)
    )
)

# Gemfile.lock contains both locked depedencies and manifest dependencies
# Ignore everything until GEM, indicating locked dependencies
# Then ignore everything until DEPENDENCIES, indicating manifest dependencies
gemfile = (
    any_char.until(string("GEM\n"), consume_other=True)
    >> string("  remote: ")
    >> any_char.until(string("\n"), consume_other=True)
    >> string("  specs:\n")
    >> mark_line(package | consume_line)
    .sep_by(string("\n"))
    .bind(
        lambda deps: string("\n\n")
        >> any_char.until(string("DEPENDENCIES\n"), consume_other=True)
        >> (manifest_package.sep_by(string("\n")) << any_char.many()).bind(
            lambda manifest: success((deps, set(manifest)))
        )
    )
)


def parse_gemfile(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    deps_opt = safe_path_parse(lockfile_path, gemfile)
    if not deps_opt:
        return []
    deps, manifest_deps = deps_opt
    output = []
    for line_number, dep in deps:
        if not dep:
            continue
        output.append(
            FoundDependency(
                package=dep[0],
                version=dep[1],
                ecosystem=Ecosystem(Gem()),
                allowed_hashes={},
                transitivity=transitivity(manifest_deps, [dep[0]]),
                line_number=line_number,
            )
        )
    return output
