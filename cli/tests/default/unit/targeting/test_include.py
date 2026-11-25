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
                "/foo/bar/baz/a.py",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/foo.py",
                "/baz.py",
                "/foo.py",
                "/foo/bar.py",
            ],
            id="All_python_files",
        ),
        pytest.param(
            ["*.go"],
            [
                "/bar/foo/baz/bar.go",
                "/baz.go",
                "/foo.go",
                "/foo/bar.go",
            ],
            id="All_go_files",
        ),
        pytest.param(
            ["*.go", "*.java"],
            [
                "/bar/foo/baz/bar.go",
                "/baz.go",
                "/baz.java",
                "/foo.go",
                "/foo.java",
                "/foo/bar.go",
                "/foo/bar.java",
            ],
            id="All_go_and_java_files",
        ),
        pytest.param(
            ["foo"],
            [
                "/foo/bar/baz/a.py",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
                "/bar/foo/baz/bar.go",
                "/bar/foo/foo.py",
                "/baz/foo",
                "/foo",
                "/foo/bar.go",
                "/foo/bar.java",
                "/foo/bar.py",
            ],
            id="All_files_named_foo_or_in_a_foo_directory_ancestor",
        ),
        pytest.param(
            ["bar/baz"],
            [
                "/bar/baz",
                "/bar/baz/foo/a.py",
                "/bar/baz/foo/b.py",
                "/bar/baz/foo/c.py",
                "/bar/baz/qux/foo/a.py",
            ],
            id="All_files_with_root_ancestor_named_bar/baz",
        ),
        pytest.param(
            ["foo/*.go"],
            [
                "/foo/bar.go",
            ],
            id="All_go_files_with_direct_root_ancestor_named_foo",
        ),
        pytest.param(
            ["foo/**/*.go"],
            [
                "/foo/bar.go",
            ],
            id="All_go_files_with_root_ancestor_named_foo",
        ),
        pytest.param(
            ["???.py"],
            [
                "/bar/foo/foo.py",
                "/baz.py",
                "/foo.py",
                "/foo/bar.py",
            ],
            id="All_py_files_with_three-characters_name",
        ),
        pytest.param(
            ["baz/qux"],
            [],
            id="ambiguous_anchored_unanchored",
        ),
    ],
)
def test_filter_include(patterns, expected_kept):
    """Test the filter_includes method on a fake list of target files"""
    actual = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
    ).filter_includes(rule_id="foo", includes=patterns, candidates=CANDIDATES)
    expected_kept = fake_targets_of_paths(Path(name) for name in expected_kept)
    # Warning: an extra file on the left is shown as '+' (added)!
    assert actual.kept == expected_kept, "expected selection != actual selection"
    assert (
        actual.removed == CANDIDATES - actual.kept
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


def _test_filter_include_equivalent_variants(pattern: str, pattern_variant: str):
    """Test some different variations of the pattern yield the same result."""
    expected_result = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
    ).filter_includes(rule_id="foo", includes=[pattern], candidates=CANDIDATES)
    actual_result = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
    ).filter_includes(rule_id="foo", includes=[pattern_variant], candidates=CANDIDATES)
    assert actual_result == expected_result


@pytest.mark.quick
def test_filter_include_anchored_variants():
    for pattern_variant in EQUIVALENT_ANCHORED_PATTERNS:
        _test_filter_include_equivalent_variants(
            EQUIVALENT_ANCHORED_PATTERNS[0], pattern_variant
        )


@pytest.mark.quick
def test_filter_include_floating_variants():
    for pattern_variant in EQUIVALENT_FLOATING_PATTERNS:
        _test_filter_include_equivalent_variants(
            EQUIVALENT_FLOATING_PATTERNS[0], pattern_variant
        )


@pytest.mark.quick
@pytest.mark.parametrize("is_include", [True, False])
@pytest.mark.parametrize(
    "pattern, matching_paths",
    [
        ("/test.py", {"/test.py"}),
        ("test.py", {"/test.py", "/a/test.py", "/x/a/test.py"}),
        ("a/test.py", {"/a/test.py"}),
        ("/a/test.py", {"/a/test.py"}),
        ("**/a/test.py", {"/a/test.py", "/x/a/test.py"}),
        ("a/", {"/a/test.py", "/x/a/test.py"}),
    ],
)
def test_rule_path_anchoring(is_include, pattern, matching_paths, caplog):
    candidates = fake_targets_of_paths(
        Path(name) for name in ["/test.py", "/a/test.py", "/x/a/test.py"]
    )
    manager = TargetManager(scanning_root_strings=frozenset([Path(".")]))
    if is_include:
        result = manager.filter_includes(
            rule_id="test", includes=[pattern], candidates=candidates
        )
    else:
        result = manager.filter_excludes(
            rule_id="test", excludes=[pattern], candidates=candidates
        )
    matching = fake_targets_of_paths(Path(name) for name in matching_paths)
    assert result.kept == (matching if is_include else candidates - matching)
    assert "will soon be interpreted" not in caplog.text
