#
# Copyright (c) 2026 Semgrep Inc.
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

from semgrep.rule_lang import convert_config_id_to_prefix
from semgrep.rule_lang import has_patterns_key
from semgrep.rule_lang import prepend_rule_path
from semgrep.rule_lang import project_depends_on
from semgrep.rule_lang import safe_relative_to
from semgrep.rule_lang import sanitize_rule_id_fragment


@pytest.mark.quick
@pytest.mark.parametrize(
    "raw,expected",
    [
        ("my-rule", "my-rule"),
        ("my.rule-1_x", "my.rule-1_x"),
        ("my rule!", "myrule"),
        ("a/b\\c:d", "abcd"),
        ("", ""),
        ("ågård", "grd"),
    ],
)
def test_sanitize_rule_id_fragment(raw, expected):
    assert sanitize_rule_id_fragment(raw) == expected


@pytest.mark.quick
def test_safe_relative_to():
    assert safe_relative_to(Path("a/b"), Path("a")) == Path("b")
    # no common prefix: returned unchanged instead of raising
    assert safe_relative_to(Path("/x/y"), Path("/a/b")) == Path("/x/y")


@pytest.mark.quick
def test_has_patterns_key():
    assert has_patterns_key({"pattern": "foo($X)"}) is True
    assert has_patterns_key({"patterns": []}) is True
    assert has_patterns_key({"id": "x", "message": "m"}) is False
    assert has_patterns_key({}) is False


@pytest.mark.quick
def test_project_depends_on():
    assert project_depends_on({}) == []
    assert project_depends_on({"r2c-internal-project-depends-on": {"a": "b"}}) == [
        {"a": "b"}
    ]
    either = [{"a": "b"}, {"c": "d"}]
    assert (
        project_depends_on(
            {"r2c-internal-project-depends-on": {"depends-on-either": either}}
        )
        == either
    )


@pytest.mark.quick
@pytest.mark.parametrize(
    "filename,rule_id,expected",
    [
        (None, "foo", "foo"),
        ("", "foo", "foo"),
        (None, "", "no-rule-id"),
        ("a/b/c.yml", "foo", "a.b.foo"),
    ],
)
def test_prepend_rule_path(filename, rule_id, expected):
    assert prepend_rule_path(filename, rule_id) == expected


@pytest.mark.quick
def test_convert_config_id_to_prefix(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    assert convert_config_id_to_prefix("a/b/c.yml") == "a.b."
    # absolute paths outside the cwd keep their (sanitized) directories
    assert convert_config_id_to_prefix("/x/y/c.yml") == "x.y."
