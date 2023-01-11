from pathlib import Path
from typing import List
from typing import Optional

from packaging.version import InvalidVersion
from packaging.version import Version
from parsy import any_char
from parsy import Parser
from parsy import string
from parsy import success

from semdep.parsers.util import consume_line
from semdep.parsers.util import consume_word
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


dep = mark_line(
    upto([":"], consume_other=True)
    >> upto([":"], consume_other=True).bind(
        lambda package: upto(["="]).bind(
            lambda version: consume_line >> success((package, version))
        )
    )
)

PREFIX = """\
# This is a Gradle generated file for dependency locking.
# Manual edits can break the build and are not advised.
# This file is expected to be part of source control.
"""


# If we hit a line that isn't simple, like this:
#     implementation fileTree(dir: "libs", include: ["*.jar"])
# just ignore it
manifest_line: "Parser[Optional[str]]" = (
    (string("\t") | string("  "))
    >> consume_word
    >> string(" ")
    >> any_char.bind(
        lambda next: (success(None) << consume_line)
        if next not in ['"', "'"]
        else upto([":"], consume_other=True)
        >> upto([":", "'", '"']).bind(lambda package: success(package) << consume_line)  # type: ignore
    )
)

manifest = (
    any_char.until(string("dependencies {\n"), consume_other=True)
    >> (manifest_line | success(None))
    .sep_by(string("\n"), min=1)
    .map(lambda xs: {x for x in xs if x})
    << any_char.many()
)

gradle = string(PREFIX) >> (dep | (string("empty=") >> consume_line)).sep_by(
    string("\n")
).map(lambda xs: [x for x in xs if x])


def parse_gradle(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    deps = safe_path_parse(lockfile_path, gradle)
    if not deps:
        return []
    manifest_deps = safe_path_parse(manifest_path, manifest)
    output = []
    for line_number, (package, version) in deps:
        try:
            Version(version)
        except InvalidVersion:
            logger.info("No valid version found for {name}")
            continue
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Maven()),
                resolved_url=None,
                allowed_hashes={},
                transitivity=transitivity(manifest_deps, [package]),
                line_number=line_number,
            )
        )
    return output


text = """\
plugins {
  id "com.github.mxenabled.coppuccino" version "3.2.1"
  id "com.github.mxenabled.vogue" version "1.0.2"
  id "java"
  id "maven-publish"
  id "groovy"
  id "io.freefair.lombok" version "6.4.3"
}

group "com.mx.path.example"
version "0.0.1"
jar.archiveBaseName = "path-examples"

allprojects {
  repositories {
    mavenCentral()
    mavenLocal()
  }
}

sourceCompatibility = 1.8
targetCompatibility = 1.8

dependencies {
  implementation platform("com.mx.path-facilities:platform:[1.0,2.0[")
  implementation platform("com.mx.path-mdx-model:platform:[1.0,2.0[")
  implementation "com.mx.path-core:http"
  implementation "com.mx.path-facilities:message-broker-nats"
  implementation "com.mx.path-mdx-model:mdx-gateways"

  implementation "info.picocli:picocli:4.6.3" // for commandline features
  implementation "commons-io:commons-io:20030203.000550" // for file reading
  implementation "org.slf4j:slf4j-nop:[1.7.0,1.8[" // silence log messages
  implementation "io.opentracing:opentracing-mock:0.33.0" // for request tracing

  testImplementation "org.mockito:mockito-inline:[4.0,5.0["
  testImplementation "org.spockframework:spock-core:[2.0,3.0["
  testImplementation "org.junit.jupiter:junit-jupiter-api:[5.0,6.0["
}

sourceSets {
  main {
    java {
      exclude "path/e100_spring_app/**"
    }
  }
}

artifacts { archives jar }

wrapper {
  gradleVersion = "7.4.2"
  distributionType = Wrapper.DistributionType.ALL
}

jar {
  duplicatesStrategy = "exclude"

  from {
    (configurations.runtimeClasspath).collect { it.isDirectory() ? it : zipTree(it) }
  } {
    // This removes signatures from signed JARs.
    // For some reason, the JNI will fail the signature check when signed JARs are included in an UberJar.
    exclude "META-INF/*.RSA", "META-INF/*.SF", "META-INF/*.DSA"
  }
}
"""
