#
# Copyright (c) 2024-2025 Semgrep Inc.
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
def test_metavariable_comparison_rule(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_base(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison-base.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_strip(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison-strip.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_bad_content(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison-bad-content.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_propagation_comparison(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-comparison-propagation.yaml",
            target_name="metavariable_propagation/metavariable-comparison-propagation.py",
        ).stdout,
        "results.json",
    )
