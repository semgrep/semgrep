"""
Parsers for pnpm-lock.yaml files
Based on https://github.com/pnpm/spec/blob/master/lockfile/5.2.md
"""

from semdep.external.parsy import string, string_from, whitespace
from semdep.parsers.util import mark_line, upto
from semdep.external.parsy import success
from semdep.parsers.util import consume_line


package = whitespace.optional() >> upto(":")

version_specifier = upto("\n").bind(
    lambda version: success(
        version if "*" not in version else None
    )
)

dep = package.bind(
    lambda package: whitespace.optional()
    >> (
        string(":") >>
        whitespace.optional() >>
        string("^").optional() >>
        version_specifier.sep_by(string(",") >> whitespace.optional()).bind(
            lambda version_specifiers: upto("\n").optional()
            >> success((package, [x for x in version_specifiers if x]))
        )
    )
)

dependencies_header = whitespace.optional() >> string("specifiers:")

direct_dependencies = (
    dependencies_header >>
    whitespace.optional() >>
    mark_line(dep)
    .sep_by(string("\n").at_least(1))
    .map(lambda xs: [(l, x) for (l, x) in xs if x])
)