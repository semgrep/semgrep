from typing import List
from typing import Optional

from parsy import any_char
from parsy import string
from parsy import success

from semdep.parsers.util import consume_line
from semdep.parsers.util import mark_line
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gem

version = string("(") >> upto([")"], consume_other=True)

package = string("    ") >> upto([" "], consume_other=True).bind(
    lambda package: version.bind(lambda version: success((package, version)))
)

manifest_package = string("  ") >> upto([" ", "!"]).bind(
    lambda package: any_char.bind(
        lambda next: success(package) if next == "!" else version >> success(package)
    )
)

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
        >> manifest_package.sep_by(string("\n"))
        .map(lambda xs: set(xs))
        .bind(lambda manifest: success((deps, manifest)))
    )
)


def parse_gemfile(
    lockfile_text: str, manifest_text: Optional[str]
) -> List[FoundDependency]:
    deps, manifest_deps = gemfile.parse(lockfile_text)
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


text = """\
PATH
  remote: .
  specs:
    faker (2.20.0)
      i18n (>= 1.8.11, < 2)

GEM
  remote: https://rubygems.org/
  specs:
    ast (2.4.2)
    coderay (1.1.3)
    concurrent-ruby (1.1.9)
    docile (1.3.2)
    i18n (1.8.11)
      concurrent-ruby (~> 1.0)
    json (2.3.0)
    method_source (1.0.0)
    minitest (5.15.0)
    parallel (1.21.0)
    parser (3.1.0.0)
      ast (~> 2.4.1)
    power_assert (2.0.1)
    pry (0.14.1)
      coderay (~> 1.1)
      method_source (~> 1.0)
    rainbow (3.1.1)
    rake (13.0.6)
    regexp_parser (2.2.0)
    rexml (3.2.5)
    rubocop (1.25.0)
      parallel (~> 1.10)
      parser (>= 3.1.0.0)
      rainbow (>= 2.2.2, < 4.0)
      regexp_parser (>= 1.8, < 3.0)
      rexml
      rubocop-ast (>= 1.15.1, < 2.0)
      ruby-progressbar (~> 1.7)
      unicode-display_width (>= 1.4.0, < 3.0)
    rubocop-ast (1.15.1)
      parser (>= 3.0.1.1)
    ruby-progressbar (1.11.0)
    simplecov (0.17.1)
      docile (~> 1.1)
      json (>= 1.8, < 3)
      simplecov-html (~> 0.10.0)
    simplecov-html (0.10.2)
    test-unit (3.5.3)
      power_assert
    timecop (0.9.4)
    unicode-display_width (2.1.0)
    webrick (1.7.0)
    yard (0.9.27)
      webrick (~> 1.7.0)

PLATFORMS
  ruby

DEPENDENCIES
  faker!
  minitest (= 5.15.0)
  pry (= 0.14.1)
  rake (= 13.0.6)
  rubocop (= 1.25.0)
  simplecov (= 0.17.1, < 0.18)
  test-unit (= 3.5.3)
  timecop (= 0.9.4)
  yard (= 0.9.27)

BUNDLED WITH
   2.1.4
"""
