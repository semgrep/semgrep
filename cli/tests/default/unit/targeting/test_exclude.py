from pathlib import Path

import pytest

from semgrep.target_manager import TargetManager
from semgrep.types import fake_targets_of_paths

# ppaths: all start with a slash
CANDIDATE_NAMES = [
    "/foo/bar/baz/a.py",
    "/bar/baz",
    "/bar/baz/foo/a.py",
    "/bar/baz/foo/b.py",
    "/bar/baz/foo/c.py",
    "/bar/baz/qux/foo/a.py",
    "/bar/foo/baz/bar.go",
    "/bar/foo/foo.py",
    "/baz.go",
    "/baz.java",
    "/baz.py",
    "/baz/foo",
    "/foo",
    "/foo.go",
    "/foo.java",
    "/foo.py",
    "/foo/bar.go",
    "/foo/bar.java",
    "/foo/bar.py",
]
CANDIDATES = fake_targets_of_paths(Path(name) for name in CANDIDATE_NAMES)


@pytest.mark.quick
@pytest.mark.parametrize("legacy_behavior", [True, False], ids=["legacy", "standard"])
@pytest.mark.parametrize(
    "patterns, expected_kept, expected_kept_legacy",
    [
        pytest.param(
            ["*.py"],
            [
                "/bar/baz",
                "/bar/foo/baz/bar.go",
                "/baz.go",
                "/baz.java",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo/bar.go",
                "/foo/bar.java",
            ],
            None,
            id="All_python_files",
        ),
        pytest.param(
            ["*.go"],
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/foo.py",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.java",
                "/foo.py",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            None,
            id="All_go_files",
        ),
        pytest.param(
            ["*.go", "*.java"],
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/foo.py",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.py",
                "/foo/bar.py",
            ],
            None,
            id="All_go_and_java_files",
        ),
        pytest.param(
            ["foo"],
            [
                "/bar/baz",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/foo.go",
                "/foo.java",
                "/foo.py",
            ],
            None,
            id="All_files_named_foo_or_in_a_foo_directory_ancestor",
        ),
        pytest.param(
            ["bar/baz"],
            [
                # not a match because bar/baz is anchored:
                "/foo/bar/baz/a.py",
                "/bar/foo/baz/bar.go",
                "/bar/foo/foo.py",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo.py",
                "/foo/bar.go",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            # legacy:
            [
                "/bar/foo/baz/bar.go",
                "/bar/foo/foo.py",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo.py",
                "/foo/bar.go",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            id="All_files_with_an_ancestor_named_bar/baz",
        ),
        pytest.param(
            ["foo/*.go"],
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/baz/bar.go",
                "/bar/foo/foo.py",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo.py",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            None,
            id="All_go_files_with_a_direct_ancestor_named_foo",
        ),
        pytest.param(
            ["foo/**/*.go"],
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                # pattern is anchored, not matching this:
                "/bar/foo/baz/bar.go",
                "/bar/foo/foo.py",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo.py",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            # legacy:
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/foo.py",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo.py",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            id="All_go_files_with_an_ancestor_named_foo",
        ),
        pytest.param(
            ["???.py"],
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/baz/bar.go",
                "/baz.go",
                "/baz.java",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo/bar.go",
                "/foo/bar.java",
            ],
            None,
            id="All_py_files_with_three-characters_name",
        ),
        pytest.param(
            # Different results depending on legacy or new behavior
            ["baz/qux"],
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                # excluded in legacy implementation only:
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/baz/bar.go",
                "/bar/foo/foo.py",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo.py",
                "/foo/bar.go",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            # Special expectations for legacy behavior:
            [
                "/foo/bar/baz/a.py",
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                # excluded in legacy implementation only:
                # "/bar/baz/qux/foo/a.py",
                "/bar/foo/baz/bar.go",
                "/bar/foo/foo.py",
                "/baz.go",
                "/baz.java",
                "/baz.py",
                "/baz/foo",
                "/foo",
                "/foo.go",
                "/foo.java",
                "/foo.py",
                "/foo/bar.go",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            id="ambiguous_anchored_unanchored",
        ),
    ],
)
def test_filter_exclude(
    patterns, expected_kept, expected_kept_legacy, legacy_behavior: bool
):
    expected_kept = (
        expected_kept_legacy
        if legacy_behavior and expected_kept_legacy is not None
        else expected_kept
    )
    actual = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
        legacy_rule_filtering=legacy_behavior,
    ).filter_excludes(rule_id="foo", excludes=patterns, candidates=CANDIDATES)
    expected_kept = fake_targets_of_paths(Path(name) for name in expected_kept)
    print(f"expected selection: {sorted([str(x.ppath) for x in expected_kept])}")
    print(f"actual selection: {sorted([str(x.ppath) for x in actual.kept])}")
    # Warning: an extra file on the left is shown as '+' (added)!
    assert actual.kept == expected_kept, "expected selection != actual selection"
    assert (
        actual.kept == CANDIDATES - actual.removed
    ), "the union of selected and deselected paths should equal the original set!"


EQUIVALENT_ANCHORED_PATTERNS = [
    "baz/qux",
    "/baz/qux",
    "baz/qux/",
    "/baz/qux/",
    "baz/qux/**",
]

EQUIVALENT_ANCHORED_PATTERNS_LEGACY = [
    "/baz/qux",
    "/baz/qux/",
]

EQUIVALENT_FLOATING_PATTERNS = [
    # unanchored
    "qux",
    "qux/",
    # anchored but floating
    "**/qux",
    "**/qux/",
    "/**/qux",
    "/**/qux/",
]


def _test_filter_exclude_equivalent_variants(
    pattern: str, pattern_variant: str, legacy_filtering: bool
):
    """Test some different variations of the pattern yield the same result."""
    expected_result = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
        legacy_rule_filtering=legacy_filtering,
    ).filter_excludes(rule_id="foo", excludes=[pattern], candidates=CANDIDATES)
    actual_result = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
        legacy_rule_filtering=legacy_filtering,
    ).filter_excludes(rule_id="foo", excludes=[pattern_variant], candidates=CANDIDATES)
    assert actual_result == expected_result


@pytest.mark.quick
@pytest.mark.parametrize(
    "pattern_variants, legacy_filtering",
    [
        (EQUIVALENT_ANCHORED_PATTERNS, False),
        (EQUIVALENT_ANCHORED_PATTERNS_LEGACY, True),
    ],
    ids=["standard", "legacy"],
)
def test_filter_exclude_anchored_variants(
    pattern_variants: str, legacy_filtering: bool
):
    reference_pattern = (
        EQUIVALENT_ANCHORED_PATTERNS_LEGACY[0]
        if legacy_filtering
        else EQUIVALENT_ANCHORED_PATTERNS[0]
    )
    for pattern_variant in pattern_variants:
        _test_filter_exclude_equivalent_variants(
            reference_pattern, pattern_variant, legacy_filtering=legacy_filtering
        )


@pytest.mark.quick
@pytest.mark.parametrize(
    "pattern_variants, legacy_filtering",
    [
        (EQUIVALENT_FLOATING_PATTERNS, False),
        (EQUIVALENT_FLOATING_PATTERNS, True),
    ],
)
def test_filter_exclude_floating_variants(
    pattern_variants: str, legacy_filtering: bool
):
    reference_pattern = EQUIVALENT_FLOATING_PATTERNS[0]
    for pattern_variant in pattern_variants:
        _test_filter_exclude_equivalent_variants(
            reference_pattern, pattern_variant, legacy_filtering=legacy_filtering
        )
