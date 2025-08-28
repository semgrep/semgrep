#
# Copyright (c) 2020-2025 Semgrep Inc.
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
import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/spacegrep/html.yaml", "spacegrep/html.mustache"),
        ("rules/spacegrep/markdown.yaml", "spacegrep/markdown.md"),
        ("rules/spacegrep/httpresponse.yaml", "spacegrep/httpresponse.txt"),
        ("rules/spacegrep/dockerfile.yaml", "spacegrep/root.Dockerfile"),
        ("rules/spacegrep/dockerfile.yaml", "spacegrep/dockerfile"),
        ("rules/spacegrep/multi-lines.yaml", "spacegrep/multi-lines.java"),
        ("rules/spacegrep/terraform.yaml", "spacegrep/terraform.tf"),
    ],
)
def test_spacegrep(run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule, target):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/spacegrep/nosem-html.yaml", "spacegrep/nosem.html"),
    ],
)
def test_spacegrep_nosem(run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule, target):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            rule, target_name=target, options=["--no-rewrite-rule-ids"]
        ).stdout,
        "results.json",
    )
