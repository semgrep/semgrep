import textwrap
from pathlib import Path

import pytest

from semdep.parsers import swiftpm
from semdep.parsers.util import json_doc
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import PPackageResolved
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import SwiftPM
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@pytest.mark.parametrize(
    "original, output",
    [
        ('url: "https://example.com/example-package.git"', "example-package"),
        ('url: "git@github.com:user/project1.git"', "project1"),
        ('url: "https://github.com/user/project2.git"', "project2"),
        ('url: "http://github.com/user/project3.git"', "project3"),
        ('url: "git@192.168.101.127:user/project4.git"', "project4"),
        ('url: "https://github.com/user/project5"', "project5"),
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_url_block_parser(original: str, output: str):
    result = swiftpm.url_block.parse(original)
    assert result == output


@pytest.mark.parametrize(
    "original, output",
    [
        ('name: "example-package"', "example-package"),
        ('name:   "project1"', "project1"),
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_name_block_parser(original: str, output: str):
    result = swiftpm.name_block.parse(original)
    assert result == output


@pytest.mark.parametrize(
    "original",
    [
        'from: "1.2.3"',
        'from: "3.2.1"',
        'from: "2.0a"',
        'from: "1.0-SNAPSHOT"',
        'from: "nightly"',
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_from_block_parser(original: str):
    result = swiftpm.from_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize("original", ['"1.2.3"..<"1.2.6"', '"7.2.3"..<"11.2.6"'])
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_range_block_parser(original: str):
    result = swiftpm.range_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize(
    "original",
    [
        '.exact("1.2.3")',
        '.exact("1.3-testing")',
        'exact: "1.2.3"',
        'exact:   "1.3-testing"',
        "exact: Version(1, 2, 3)",
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_exact_block_parser(original: str):
    result = swiftpm.exact_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize("original", ['path: "foo/bar"', 'path:  "ahhh"'])
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_path_block_parser(original: str):
    result = swiftpm.path_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize(
    "original",
    [
        '.revision("f74b07278b926c9ec6f9643455ea00d1ce04a333")',
        '.revision("e74b07278b926c9ec6f9643455ea00d1ce04a021")',
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_revision_block_parser(original: str):
    result = swiftpm.revision_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize(
    "original",
    [
        '.upToNextMajor(from: "1.2.3")',
        '.upToNextMajor(from:"1.2.3")',
        '.upToNextMajor( from: "1.2.3")',
        '.upToNextMajor(from: "1.0-test")',
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_up_to_next_major_block_parser(original: str):
    result = swiftpm.up_to_next_major_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize(
    "original",
    [
        '.upToNextMinor(from: "1.2.3")',
        '.upToNextMinor(from:"1.2.3")',
        '.upToNextMinor( from: "1.2.3")',
        '.upToNextMinor(from: "1.0-test")',
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_up_to_next_minor_block_parser(original: str):
    result = swiftpm.up_to_next_minor_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize(
    "original", ['.branch("develop")', '.branch("jl/testing-branch")']
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_branch_block_parser(original: str):
    result = swiftpm.branch_block.parse_partial(original)
    assert result[0] == original
    assert not result[1]


@pytest.mark.parametrize(
    "original, expected",
    [
        (
            '.package(url: "https://github.com/repo/package.git", .upToNextMajor(from: "7.8.0"))',
            "package",
        ),
        (
            '.package(url: "https://github.com/repo/package.git", .upToNextMinor(from: "7.8.0"))',
            "package",
        ),
        (
            '.package(url: "git://github.com/repo/package.git", from: "7.8.0")',
            "package",
        ),
        (
            '.package(url: "https://github.com/repo/package.git", .branch("develop"))',
            "package",
        ),
        (
            '.package(url: "https://github.com/repo/package.git", .revision("e74b07278b926c9ec6f9643455ea00d1ce04a021"))',
            "package",
        ),
        (
            '.package(url: "https://github.com/repo/package.git", .exact("7.8.0"))',
            "package",
        ),
        (
            '.package(url: "https://github.com/repo/package", exact: "7.8.0")',
            "package",
        ),
        (
            '.package(name: "package", path: "foo/bar")',
            "package",
        ),
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_package_block_parser(original: str, expected: str):
    result = swiftpm.package_block.parse_partial(original)
    assert result[0][1] == expected
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_manifest_parser():
    file_str = textwrap.dedent(
        """// swift-tools-version: 5.9
    // The swift-tools-version declares the minimum version of Swift required to build this package.

    import PackageDescription

    let package = Package(
        name: "spm",
        products: [
            // Products define the executables and libraries a package produces, making them visible to other packages.
            .library(
                name: "spm",
                targets: ["spm"]),
        ],
        dependencies: [
            .package(url: "https://github.com/repo_1/package_1.git", .upToNextMajor(from: "7.8.0"))
            ,
            .package(url: "https://github.com/repo_2/package_2.git", .upToNextMajor(from: "7.8.0")) ,
            .package(
                url: "https://github.com/repo_3/package_3.git",
                from: "0.2.0"
            ),
            //.package(url: "https://github.com/repo_4/package_4.git", .upToNextMajor(from: "7.8.0")),
            .package(url: "https://github.com/repo_5/package_5.git", .upToNextMajor(from: "7.8.0"))
        ],
        targets: [
            // Targets are the basic building blocks of a package, defining a module or a test suite.
            // Targets can depend on other targets in this package and products from dependencies.
            .target(
                name: "spm"),
            .testTarget(
                name: "spmTests",
                dependencies: ["spm"]),
        ]
    )
    """
    )

    result = swiftpm.package_swift_parser.parse_partial(file_str)
    assert result[0] == [
        (15, "package_1"),
        (17, "package_2"),
        (19, "package_3"),
        None,
        (23, "package_5"),
    ]
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_multiple_package_blocks_parser():
    deps = (
        '.package(url: "https://github.com/repo_1/package_1.git/", from: "7.8.1"),\n'
        + '.package(url: "https://github.com/repo_2/package_2.git", from: "7.8.2"),\n'
        + '.package(url: "https://github.com/repo_2/package_3.git", from: "7.8.3"),\n'
        + '.package(url: "https://github.com/repo_2/package_4.git", from: "7.8.4"),'
    )

    result = swiftpm.multiple_package_blocks.parse_partial(deps)
    assert not result[1]
    assert result[0] == [
        (1, "package_1"),
        (2, "package_2"),
        (3, "package_3"),
        (4, "package_4"),
    ]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_dependencies_block_parser():
    deps = textwrap.dedent(
        """dependencies: [
            .package(url: "https://github.com/repo_1/package_1.git", .upToNextMajor(from: "7.8.0")),
            .package(url: "https://github.com/repo_2/package_2.git", .upToNextMajor(from: "7.8.0")),
            .package(url: "https://github.com/repo_3/package_3.git", .upToNextMajor(from: "7.8.0")),
        ]
        """
    )

    result = swiftpm.dependencies_block.parse_partial(deps)
    assert result[0] == [(2, "package_1"), (3, "package_2"), (4, "package_3")]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_lockfile_v1_parser():
    lockfile_path = Path("test/fixtures/swiftpm-lock-v1/Package.resolved")
    # The final package has no version, so we ignore it, since we have no way to do version comparison on it
    lockfile_v1 = json_doc.parse(
        """
    {
      "object": {
        "pins": [
          {
            "package": "Curry",
            "repositoryURL": "https://github.com/thoughtbot/Curry.git",
            "state": {
              "branch": null,
              "revision": "4331dd50bc1db007db664a23f32e6f3df93d4e1a",
              "version": "4.0.2"
            }
          },
          {
            "package": "PrettyColors",
            "repositoryURL": "https://github.com/jdhealy/PrettyColors.git",
            "state": {
              "branch": null,
              "revision": "afd4553a4db6f656521cfe9b1f70bece2748c7d8",
              "version": "5.0.2"
            }
          },
          {
          "package": "Stencil",
          "repositoryURL": "https://github.com/stencilproject/Stencil.git",
          "state": {
              "branch": null,
              "revision": "9c3468e300ba75ede0d7eb4c495897e0ac3591c3",
              "version": null
            }
          }
        ]
      },
      "version": 1
    }
    """
    ).as_dict()

    found_deps, errors = swiftpm.parse_swiftpm_v1(
        lockfile_path, lockfile_v1, {""}, manifest_path=None
    )
    expected_deps = [
        FoundDependency(
            package="curry",
            version="4.0.2",
            ecosystem=Ecosystem(SwiftPM()),
            allowed_hashes={},
            transitivity=Transitivity(Transitive()),
            line_number=11,
            git_ref="4331dd50bc1db007db664a23f32e6f3df93d4e1a",
            resolved_url="https://github.com/thoughtbot/Curry.git",
            lockfile_path=Fpath(str(lockfile_path)),
            manifest_path=None,
        ),
        FoundDependency(
            package="prettycolors",
            version="5.0.2",
            ecosystem=Ecosystem(SwiftPM()),
            allowed_hashes={},
            transitivity=Transitivity(Transitive()),
            line_number=20,
            git_ref="afd4553a4db6f656521cfe9b1f70bece2748c7d8",
            resolved_url="https://github.com/jdhealy/PrettyColors.git",
            lockfile_path=Fpath(str(lockfile_path)),
            manifest_path=None,
        ),
    ]
    expected_errors = [
        DependencyParserError(
            path=Fpath(
                value="test/fixtures/swiftpm-lock-v1/Package.resolved",
            ),
            parser=ScaParserName(
                value=PPackageResolved(),
            ),
            reason="Unable to determine version of dependency - stencil - skipping. This may be because the dependency is pinned to an unreleased commit.",
            line=29,
        ),
    ]

    assert found_deps == expected_deps
    assert errors == expected_errors


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_lockfile_v2_parser():
    lockfile_path = Path("test/fixtures/swiftpm-lock-v2/Package.resolved")
    # The final package has no version, so we ignore it, since we have no way to do version comparison on it
    lockfile_v2 = json_doc.parse(
        """
    {
      "pins" : [
        {
          "identity" : "bson",
          "kind" : "remoteSourceControl",
          "location" : "https://github.com/orlandos-nl/BSON.git",
          "state" : {
            "revision" : "944dfb3b0eb028f477c25ba6a071181de8ab903a",
            "version" : "8.0.10"
          }
        },
        {
          "identity" : "dnsclient",
          "kind" : "remoteSourceControl",
          "location" : "https://github.com/orlandos-nl/DNSClient.git",
          "state" : {
            "revision" : "770249dcb7259c486f2d68c164091b115ccb765f",
            "version" : "2.2.1"
            }
          },
          {
          "identity": "Stencil",
          "kind": "remoteSourceControl",
          "location": "https://github.com/stencilproject/Stencil.git",
          "state": {
              "revision": "9c3468e300ba75ede0d7eb4c495897e0ac3591c3",
              "version": null
            }
          }
      ],
      "version": 2
    }
    """
    ).as_dict()

    found_deps, errors = swiftpm.parse_swiftpm_v2_v3(
        lockfile_path, lockfile_v2, {""}, manifest_path=None
    )
    expected_deps = [
        FoundDependency(
            package="bson",
            version="8.0.10",
            ecosystem=Ecosystem(SwiftPM()),
            allowed_hashes={},
            transitivity=Transitivity(Transitive()),
            line_number=10,
            git_ref="944dfb3b0eb028f477c25ba6a071181de8ab903a",
            resolved_url="https://github.com/orlandos-nl/BSON.git",
            lockfile_path=Fpath(str(lockfile_path)),
            manifest_path=None,
        ),
        FoundDependency(
            package="dnsclient",
            version="2.2.1",
            ecosystem=Ecosystem(SwiftPM()),
            allowed_hashes={},
            transitivity=Transitivity(Transitive()),
            line_number=19,
            git_ref="770249dcb7259c486f2d68c164091b115ccb765f",
            resolved_url="https://github.com/orlandos-nl/DNSClient.git",
            lockfile_path=Fpath(str(lockfile_path)),
            manifest_path=None,
        ),
    ]

    expected_errors = [
        DependencyParserError(
            path=Fpath(
                value="test/fixtures/swiftpm-lock-v2/Package.resolved",
            ),
            parser=ScaParserName(
                value=PPackageResolved(),
            ),
            reason="Unable to determine version of dependency - stencil - skipping. This may be because the dependency is pinned to an unreleased commit.",
            line=None,
            col=None,
            text=None,
        ),
    ]

    assert found_deps == expected_deps
    assert errors == expected_errors
