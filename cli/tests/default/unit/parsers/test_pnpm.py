import pytest

from semdep.parsers.pnpm import extract_base_version
from semdep.parsers.pnpm import parse_dependency_version
from semdep.parsers.pnpm import ParseResult
from semdep.parsers.pnpm import sanitize_dependency_post_v9
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyChild


@pytest.mark.quick
@pytest.mark.parametrize(
    "input_version, expected_base_version",
    [
        # Basic semantic versions
        ("1.0.0", "1.0.0"),
        ("8.2.0", "8.2.0"),
        # Semantic versions with build metadata
        ("1.0.0+build", "1.0.0+build"),
        ("8.2.0+meta", "8.2.0+meta"),
        # Semantic versions with contexts
        ("8.2.0(eslint@9.9.1)", "8.2.0"),
        ("8.2.0+build(eslint@9.9.1)", "8.2.0+build"),
        ("1.0.0+meta(context@1.2.3)", "1.0.0+meta"),
        ("8.2.0(eslint@9.9.1)(typescript@5.5.4)", "8.2.0"),
        ("8.2.0+build(eslint@9.9.1)(typescript@5.5.4)", "8.2.0+build"),
        (
            "2.8.3(@sveltejs/vite-plugin-svelte@4.0.0-next.6(svelte@packages+svelte)(vite@5.4.6(@types/node@20.12.7)(lightningcss@1.23.0)(sass@1.70.0)(terser@5.27.0)))(svelte@packages+svelte)(vite@5.4.6(@types/node@20.12.7)(lightningcss@1.23.0)(sass@1.70.0)(terser@5.27.0))",
            "2.8.3",
        ),
        ("/@pnpm/node-fetch@1.0.0", "1.0.0"),
        ("/random-path/other-package@5.0.0", "5.0.0"),
        # Edge cases
        ("", ""),  # Empty string
        ("8.2.0(", "8.2.0"),  # Malformed version with an unclosed parenthesis
        (
            "8.2.0+build(",
            "8.2.0+build",
        ),  # Malformed version with build metadata and unclosed parenthesis
        ("8.2.0()", "8.2.0"),  # Empty context
    ],
)
def test_extract_base_version(input_version, expected_base_version):
    assert extract_base_version(input_version) == expected_base_version


@pytest.mark.quick
@pytest.mark.parametrize(
    "input_dependency, expected_dependency",
    [
        # Basic dependency without alias or context
        (
            DependencyChild(package="string-width", version="4.2.3"),
            DependencyChild(package="string-width", version="4.2.3"),
        ),
        # Dependency with alias
        (
            DependencyChild(package="string-width-cjs", version="string-width@4.2.3"),
            DependencyChild(package="string-width", version="4.2.3"),
        ),
        # Dependency with alias and build metadata
        (
            DependencyChild(
                package="string-width-cjs", version="string-width@4.2.3+build"
            ),
            DependencyChild(package="string-width", version="4.2.3+build"),
        ),
        # Dependency with alias and context
        (
            DependencyChild(
                package="string-width-cjs", version="string-width@4.2.3(eslint@9.9.1)"
            ),
            DependencyChild(package="string-width", version="4.2.3"),
        ),
        # Dependency with nested contexts
        (
            DependencyChild(
                package="typescript-eslint",
                version="8.2.0(eslint@9.9.1)(typescript@5.5.4)",
            ),
            DependencyChild(package="typescript-eslint", version="8.2.0"),
        ),
        # Dependency with build metadata and nested contexts
        (
            DependencyChild(
                package="typescript-eslint",
                version="8.2.0+build(eslint@9.9.1)(typescript@5.5.4)",
            ),
            DependencyChild(package="typescript-eslint", version="8.2.0+build"),
        ),
        # Dependency with no version
        (
            DependencyChild(package="some-package", version=""),
            DependencyChild(package="some-package", version=""),
        ),
        # Dependency with empty package and version
        (
            DependencyChild(package="", version=""),
            DependencyChild(package="", version=""),
        ),
        (
            DependencyChild(
                package="react-loadable",
                version="@docusaurus/react-loadable@6.0.0(react@18.3.1)",
            ),
            DependencyChild(package="@docusaurus/react-loadable", version="6.0.0"),
        ),
        (
            DependencyChild(
                package="react-loadable-ssr-addon-v5-slorber",
                version="1.0.1(@docusaurus/react-loadable@6.0.0(react@18.3.1))(webpack@5.96.1(@swc/core@1.6.5))",
            ),
            DependencyChild(
                package="react-loadable-ssr-addon-v5-slorber", version="1.0.1"
            ),
        ),
    ],
)
def test_sanitize_dependency_post_v9(input_dependency, expected_dependency):
    assert sanitize_dependency_post_v9(input_dependency) == expected_dependency


@pytest.mark.quick
@pytest.mark.parametrize(
    "input_version, expected_result",
    [
        # Basic versions
        (
            "4.2.3",
            ParseResult(package="", version="4.2.3"),
        ),
        # Build metadata
        (
            "4.2.3+build",
            ParseResult(package="", version="4.2.3+build"),
        ),
        # Simple package@version
        (
            "string-width@4.2.3",
            ParseResult(package="string-width", version="4.2.3"),
        ),
        # Scoped packages
        (
            "@types/node@12.0.0",
            ParseResult(package="@types/node", version="12.0.0"),
        ),
        # Scoped package with build metadata
        (
            "@babel/core@7.0.0+build",
            ParseResult(package="@babel/core", version="7.0.0+build"),
        ),
        # Package with context
        (
            "10.0.4(@pnpm/logger@5.2.0)",
            ParseResult(package="", version="10.0.4"),
        ),
        # Package with multiple contexts
        (
            "8.2.0(eslint@9.9.1)(typescript@5.5.4)",
            ParseResult(package="", version="8.2.0"),
        ),
        # Scoped package with context
        (
            "@pnpm/core-loggers@10.0.4(@pnpm/logger@5.2.0)",
            ParseResult(package="@pnpm/core-loggers", version="10.0.4"),
        ),
        # Leading slash in package name
        (
            "/@pnpm/node-fetch@1.0.0",
            ParseResult(package="@pnpm/node-fetch", version="1.0.0"),
        ),
        # Empty string
        (
            "",
            ParseResult(package="", version=""),
        ),
        # Build metadata with context
        (
            "4.2.3+build(dependency@1.0.0)",
            ParseResult(package="", version="4.2.3+build"),
        ),
        # Multiple @ symbols in package name
        (
            "@org/pkg@name@1.0.0",
            ParseResult(package="@org/pkg@name", version="1.0.0"),
        ),
        # PNPM v5+ peer dependency hash suffixes
        (
            "5.13.0_33fffc354ccfa91fbe7d1677b9395a0a",
            ParseResult(package="", version="5.13.0"),
        ),
        # Scoped package with peer dependency hash
        (
            "@typescript-eslint/eslint-plugin@5.13.0_33fffc354ccfa91fbe7d1677b9395a0a",
            ParseResult(package="@typescript-eslint/eslint-plugin", version="5.13.0"),
        ),
        # With context and peer dependency hash
        (
            "5.13.0_33fffc354ccfa91fbe7d1677b9395a0a(@typescript-eslint/parser@5.0.0)",
            ParseResult(package="", version="5.13.0"),
        ),
        (
            "3.21.0_typescript@4.6.2",
            ParseResult(package="", version="3.21.0"),
        ),
    ],
)
def test_parse_dependency_version(input_version, expected_result):
    """
    Test parsing of dependency versions from pnpm-lock.yaml.

    Tests various formats that can appear in any version of pnpm-lock.yaml:
    - Basic versions (4.2.3)
    - Build metadata (4.2.3+build)
    - Package@version format (string-width@4.2.3)
    - Scoped packages (@types/node@12.0.0)
    - Context dependencies (10.0.4(@pnpm/logger@5.2.0))
    - Leading slash (/@pnpm/node-fetch@1.0.0)
    - Empty strings
    """
    assert parse_dependency_version(input_version) == expected_result
