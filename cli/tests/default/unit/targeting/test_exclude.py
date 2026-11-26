#
# Copyright (c) 2022-2025 Semgrep Inc.
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
@pytest.mark.parametrize(
    "patterns, expected_kept",
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
            id="All_py_files_with_three-characters_name",
        ),
        pytest.param(
            ["baz/qux"],
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
                "/foo/bar.go",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            id="ambiguous_anchored_unanchored",
        ),
    ],
)
def test_filter_exclude(patterns, expected_kept):
    actual = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
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


def _test_filter_exclude_equivalent_variants(pattern: str, pattern_variant: str):
    """Test some different variations of the pattern yield the same result."""
    expected_result = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
    ).filter_excludes(rule_id="foo", excludes=[pattern], candidates=CANDIDATES)
    actual_result = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
    ).filter_excludes(rule_id="foo", excludes=[pattern_variant], candidates=CANDIDATES)
    assert actual_result == expected_result


@pytest.mark.quick
def test_filter_exclude_anchored_variants():
    for pattern_variant in EQUIVALENT_ANCHORED_PATTERNS:
        _test_filter_exclude_equivalent_variants(
            EQUIVALENT_ANCHORED_PATTERNS[0], pattern_variant
        )


@pytest.mark.quick
def test_filter_exclude_floating_variants():
    for pattern_variant in EQUIVALENT_FLOATING_PATTERNS:
        _test_filter_exclude_equivalent_variants(
            EQUIVALENT_FLOATING_PATTERNS[0], pattern_variant
        )
