from pathlib import Path

import pytest

from semgrep.target_manager import TargetManager

CANDIDATE_NAMES = [
    "/foo/bar/baz/a.py",
    "bar/baz",
    "bar/baz/foo/a.py",
    "bar/baz/foo/b.py",
    "bar/baz/foo/c.py",
    "bar/baz/qux/foo/a.py",
    "bar/foo/baz/bar.go",
    "bar/foo/foo.py",
    "baz.go",
    "baz.java",
    "baz.py",
    "baz/foo",
    "foo",
    "foo.go",
    "foo.java",
    "foo.py",
    "foo/bar.go",
    "foo/bar.java",
    "foo/bar.py",
]
CANDIDATES = frozenset(Path(name) for name in CANDIDATE_NAMES)


@pytest.mark.quick
@pytest.mark.parametrize(
    "patterns, expected_kept",
    [
        pytest.param(
            ["*.py"],
            [
                "bar/baz",
                "bar/foo/baz/bar.go",
                "baz.go",
                "baz.java",
                "baz/foo",
                "foo",
                "foo.go",
                "foo.java",
                "foo/bar.go",
                "foo/bar.java",
            ],
            id="All python files",
        ),
        pytest.param(
            ["*.go"],
            [
                "/foo/bar/baz/a.py",
                "bar/baz",
                "bar/baz/foo/a.py",
                "bar/baz/foo/b.py",
                "bar/baz/foo/c.py",
                "bar/baz/qux/foo/a.py",
                "bar/foo/foo.py",
                "baz.java",
                "baz.py",
                "baz/foo",
                "foo",
                "foo.java",
                "foo.py",
                "foo/bar.java",
                "foo/bar.py",
            ],
            id="All go files",
        ),
        pytest.param(
            ["*.go", "*.java"],
            [
                "/foo/bar/baz/a.py",
                "bar/baz",
                "bar/baz/foo/a.py",
                "bar/baz/foo/b.py",
                "bar/baz/foo/c.py",
                "bar/baz/qux/foo/a.py",
                "bar/foo/foo.py",
                "baz.py",
                "baz/foo",
                "foo",
                "foo.py",
                "foo/bar.py",
            ],
            id="All go and java files",
        ),
        pytest.param(
            ["foo"],
            [
                "bar/baz",
                "baz.go",
                "baz.java",
                "baz.py",
                "foo.go",
                "foo.java",
                "foo.py",
            ],
            id="All files named foo or in a foo directory ancestor",
        ),
        pytest.param(
            ["bar/baz"],
            [
                "bar/foo/baz/bar.go",
                "bar/foo/foo.py",
                "baz.go",
                "baz.java",
                "baz.py",
                "baz/foo",
                "foo",
                "foo.go",
                "foo.java",
                "foo.py",
                "foo/bar.go",
                "foo/bar.java",
                "foo/bar.py",
            ],
            id="All files with an ancestor named bar/baz",
        ),
        pytest.param(
            ["foo/*.go"],
            [
                "/foo/bar/baz/a.py",
                "bar/baz",
                "bar/baz/foo/a.py",
                "bar/baz/foo/b.py",
                "bar/baz/foo/c.py",
                "bar/baz/qux/foo/a.py",
                "bar/foo/baz/bar.go",
                "bar/foo/foo.py",
                "baz.go",
                "baz.java",
                "baz.py",
                "baz/foo",
                "foo",
                "foo.go",
                "foo.java",
                "foo.py",
                "foo/bar.java",
                "foo/bar.py",
            ],
            id="All go files with a direct ancestor named foo",
        ),
        pytest.param(
            ["foo/**/*.go"],
            [
                "/foo/bar/baz/a.py",
                "bar/baz",
                "bar/baz/foo/a.py",
                "bar/baz/foo/b.py",
                "bar/baz/foo/c.py",
                "bar/baz/qux/foo/a.py",
                "bar/foo/foo.py",
                "baz.go",
                "baz.java",
                "baz.py",
                "baz/foo",
                "foo",
                "foo.go",
                "foo.java",
                "foo.py",
                "foo/bar.java",
                "foo/bar.py",
            ],
            id="All go files with a ancestor named foo",
        ),
        pytest.param(
            ["???.py"],
            [
                "/foo/bar/baz/a.py",
                "bar/baz",
                "bar/baz/foo/a.py",
                "bar/baz/foo/b.py",
                "bar/baz/foo/c.py",
                "bar/baz/qux/foo/a.py",
                "bar/foo/baz/bar.go",
                "baz.go",
                "baz.java",
                "baz/foo",
                "foo",
                "foo.go",
                "foo.java",
                "foo/bar.go",
                "foo/bar.java",
            ],
            id="All py files with three-characters name",
        ),
    ],
)
def test_filter_exclude(patterns, expected_kept):
    actual = TargetManager(".").filter_excludes(patterns, candidates=CANDIDATES)
    expected_kept = frozenset(Path(name) for name in expected_kept)
    assert actual.kept == expected_kept
    assert actual.kept == CANDIDATES - actual.removed


EQUIVALENT_PATTERNS = [
    "baz/qux",
    "/baz/qux",
    "baz/qux/",
    "/baz/qux/",
    "**/baz/qux",
    "baz/qux/**",
    "**/baz/qux/**",
]


@pytest.mark.quick
@pytest.mark.parametrize("pattern_variant", EQUIVALENT_PATTERNS)
def test_filter_exclude__equivalent_variants(pattern_variant):
    """Test some different variations of the pattern yield the same result."""
    expected_result = TargetManager(".").filter_excludes(
        [EQUIVALENT_PATTERNS[0]], candidates=CANDIDATES
    )
    actual_result = TargetManager(".").filter_excludes(
        [pattern_variant], candidates=CANDIDATES
    )
    assert actual_result == expected_result
