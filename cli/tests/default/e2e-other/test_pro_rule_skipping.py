#
# Copyright (c) 2023-2024 Semgrep Inc.
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


# osemgrep returns the target correctly as not scanned but pysemgrep
# marks it as scanned.
# TODO: exclude pysemfail tests or fix the problem in pysemgrep (output.py)
# See comment in Scan_subcommand.ml.
# @pytest.mark.pysemfail
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_pro_rule_skipping(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pro-rule-skipping.yaml", target_name="pro-rule-skipping/x.cls"
        ).stdout,
        "results.json",
    )


# see comment above regarding pysemfail
# @pytest.mark.pysemfail
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_pro_rule_skipping_no_parsing(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pro-rule-skipping-no-parsing.yaml",
            target_name="pro-rule-skipping-no-parsing/x.cls",
        ).stdout,
        "results.json",
    )
