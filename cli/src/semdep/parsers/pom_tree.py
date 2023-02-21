"""
Parser for maven_dep_tree.txt files, generated by maven
Based on the output of this maven plugin https://maven.apache.org/plugins/maven-dependency-plugin/tree-mojo.html
"""
from pathlib import Path
from typing import List
from typing import Optional

from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers.util import consume_line
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_path_parse
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity

# Annoying to read. In english "6 colon separated strings or 5 colon separated strings"
# Examples:
# org.apache.logging.log4j:log4j-api:jar:0.0.2:compile
# org.springframework.boot:spring-boot-configuration-processor:jar:2.3.4.RELEASE:compile (optional)
# com.google.inject:guice:jar:no_aop:4.2.2:test
dep = regex(
    "([^:\n]+:[^:\n]+):[^:\n]+:[^:\n]+:([^:\n]+):[^:\n]+", flags=0, group=(1, 2)
) | regex("([^:\n]+:[^:\n]+):[^:\n]+:([^:\n]+):[^:\n]+", flags=0, group=(1, 2))


# Examples (these would not appear in this order in a file, they're seperate):
# |  +- org.apache.maven:maven-model:jar:3.8.6:provided

# |  |  \- org.codehaus.plexus:plexus-component-annotations:jar:1.5.5:provided

# +- org.apache.logging.log4j:log4j-api:jar:0.0.2:compile

#    \- net.java.dev.jna:jna:jar:5.11.0:compile

# |     +- org.springframework:spring-aop:jar:5.3.9:compile
tree_line = mark_line(
    regex(r"((\|  )|(   ))*").bind(
        lambda depth: (regex("(\\+- )|(\\\\- )"))
        >> dep.map(
            lambda d: (
                Transitivity(Transitive() if len(depth) // 3 > 0 else Direct()),
                d[0],
                d[1],
            )
        )
    )
)


pom_tree = (
    consume_line  # First line is the name of the current project, ignore it
    >> string("\n")
    >> tree_line.sep_by(string("\n"))
    << string("\n").optional()
)


def parse_pom_tree(tree_path: Path, _: Optional[Path]) -> List[FoundDependency]:
    deps = safe_path_parse(tree_path, pom_tree)
    if not deps:
        return []
    output = []
    for line_number, (transitivity, package, version) in deps:
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Maven()),
                allowed_hashes={},
                transitivity=transitivity,
                line_number=line_number,
            )
        )
    return output


from pathlib import Path

path = Path("/Users/matthewmcquaid/test/maven_dep_tree.txt")
