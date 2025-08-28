#
# Copyright (c) 2023-2025 Semgrep Inc.
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
# Test rule ID syntax
import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/rule_id/@", "rule_id/hello.txt"),
        ("rules/rule_id/;", "rule_id/hello.txt"),
        ("rules/rule_id/@npm-style", "rule_id/hello.txt"),
    ],
)
def test_rule_id_paths(run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule, target):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target).stdout,
        "results.json",
    )
