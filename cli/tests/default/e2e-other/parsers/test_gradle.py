#
# Copyright (c) 2024-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import textwrap
from pathlib import Path

import pytest

from semdep.parsers import gradle
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct

BUILD_GRADLE_DOUBLE_QUOTED = textwrap.dedent(
    """\
    dependencies {
        implementation "org.apache.logging.log4j:log4j-core:2.14.1"
        implementation "com.fasterxml.jackson.core:jackson-databind:2.13.0"
        implementation "commons-io:commons-io:2.11.0"
    }
    """
)

BUILD_GRADLE_DOUBLE_QUOTED_NOVER = textwrap.dedent(
    """\
    dependencies {
        implementation "org.apache.logging.log4j:log4j-core"
        implementation "com.fasterxml.jackson.core:jackson-databind"
        implementation "commons-io:commons-io"
    }
    """
)

BUILD_GRADLE_SINGLE_QUOTED = textwrap.dedent(
    """\
    dependencies {
        implementation 'org.apache.logging.log4j:log4j-core:2.14.1'
        implementation 'com.fasterxml.jackson.core:jackson-databind:2.13.0'
        implementation 'commons-io:commons-io:2.11.0'
    }
    """
)

GRADLE_LOCKFILE = textwrap.dedent(
    """\
    # This is a Gradle generated file for dependency locking.
    # Manual edits can break the build and are not advised.
    # This file is expected to be part of source control.
    com.fasterxml.jackson.core:jackson-databind:2.13.0=compileClasspath,runtimeClasspath
    commons-io:commons-io:2.11.0=compileClasspath,runtimeClasspath
    org.apache.logging.log4j:log4j-core:2.14.1=compileClasspath,runtimeClasspath
    empty=
    """
)

EXPECTED_PACKAGES = {
    "org.apache.logging.log4j:log4j-core",
    "com.fasterxml.jackson.core:jackson-databind",
    "commons-io:commons-io",
}


@pytest.mark.quick
@pytest.mark.parametrize(
    "manifest",
    [
        BUILD_GRADLE_DOUBLE_QUOTED,
        BUILD_GRADLE_DOUBLE_QUOTED_NOVER,
        BUILD_GRADLE_SINGLE_QUOTED,
    ],
)
def test_gradle_manifest_parses_every_dependency(manifest: str) -> None:
    # Regression test for #12104: the version group in `manifest_line`
    # used to match across the closing quote and newlines, so with
    # double-quoted coordinates only the first dependency was parsed
    # and the rest were misclassified as transitive.
    assert gradle.manifest.parse(manifest) == EXPECTED_PACKAGES


@pytest.mark.quick
def test_gradle_double_quoted_deps_are_direct(tmp_path: Path) -> None:
    lockfile = tmp_path / "gradle.lockfile"
    lockfile.write_text(GRADLE_LOCKFILE)
    manifest = tmp_path / "build.gradle"
    manifest.write_text(BUILD_GRADLE_DOUBLE_QUOTED)

    deps, errors = gradle.parse_gradle(lockfile, manifest)

    assert not errors
    assert {dep.package for dep in deps} == EXPECTED_PACKAGES
    assert all(isinstance(dep.transitivity.value, Direct) for dep in deps)
