import pytest

from semdep.parsers.pnpm import extract_base_version
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
    ],
)
def test_sanitize_dependency_post_v9(input_dependency, expected_dependency):
    assert sanitize_dependency_post_v9(input_dependency) == expected_dependency
