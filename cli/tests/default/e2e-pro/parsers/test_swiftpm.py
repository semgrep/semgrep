import textwrap

import pytest

from semdep.parsers import swiftpm
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
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_url_block_parser(original: str, output: str):
    result = swiftpm.url_block.parse(original)
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


@pytest.mark.parametrize("original", ['.exact("1.2.3")', '.exact("1.3-testing")'])
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_swift_exact_block_parser(original: str):
    result = swiftpm.exact_block.parse_partial(original)
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
        '.package(url: "https://github.com/repo_4/package_4.git", .upToNextMajor(from: "7.8.0")),',
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
