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
import logging

import pytest
from semgrep.commands.scan import log_findings
from semgrep.engine import EngineType


@pytest.mark.quick
def test_log_findings_warns_on_suspicious_zero_with_high_jobs(caplog):
    """A clean 0-finding scan with -j > 1 should emit a warning pointing at #11606."""
    with caplog.at_level(logging.WARNING, logger="semgrep"):
        log_findings(
            {},
            EngineType.OSS,
            jobs=27,
            num_targets=4400,
            num_rules=65,
            has_errors=False,
        )
    warnings = [r for r in caplog.records if r.levelno == logging.WARNING]
    assert any("0 findings" in r.getMessage() for r in warnings)
    assert any("11606" in r.getMessage() for r in warnings)
    assert any("-j 27" in r.getMessage() for r in warnings)


@pytest.mark.quick
def test_log_findings_silent_when_serial(caplog):
    """A 0-finding scan with -j=1 should not trigger the multicore warning."""
    with caplog.at_level(logging.WARNING, logger="semgrep"):
        log_findings(
            {},
            EngineType.OSS,
            jobs=1,
            num_targets=4400,
            num_rules=65,
            has_errors=False,
        )
    warnings = [r for r in caplog.records if r.levelno == logging.WARNING]
    assert not any("11606" in r.getMessage() for r in warnings)


@pytest.mark.quick
def test_log_findings_silent_when_jobs_unspecified(caplog):
    """When jobs is None (e.g. validate path) we should not warn."""
    with caplog.at_level(logging.WARNING, logger="semgrep"):
        log_findings({}, EngineType.OSS)
    warnings = [r for r in caplog.records if r.levelno == logging.WARNING]
    assert not any("11606" in r.getMessage() for r in warnings)


@pytest.mark.quick
def test_log_findings_silent_when_errors_present(caplog):
    """If errors were reported we already alerted the user; suppress the heuristic."""
    with caplog.at_level(logging.WARNING, logger="semgrep"):
        log_findings(
            {},
            EngineType.OSS,
            jobs=27,
            num_targets=4400,
            num_rules=65,
            has_errors=True,
        )
    warnings = [r for r in caplog.records if r.levelno == logging.WARNING]
    assert not any("11606" in r.getMessage() for r in warnings)


@pytest.mark.quick
def test_log_findings_silent_when_no_targets(caplog):
    """An empty target set legitimately produces 0 findings."""
    with caplog.at_level(logging.WARNING, logger="semgrep"):
        log_findings(
            {},
            EngineType.OSS,
            jobs=27,
            num_targets=0,
            num_rules=65,
            has_errors=False,
        )
    warnings = [r for r in caplog.records if r.levelno == logging.WARNING]
    assert not any("11606" in r.getMessage() for r in warnings)


@pytest.mark.quick
def test_log_findings_silent_when_no_rules(caplog):
    """An empty rules set legitimately produces 0 findings."""
    with caplog.at_level(logging.WARNING, logger="semgrep"):
        log_findings(
            {},
            EngineType.OSS,
            jobs=27,
            num_targets=4400,
            num_rules=0,
            has_errors=False,
        )
    warnings = [r for r in caplog.records if r.levelno == logging.WARNING]
    assert not any("11606" in r.getMessage() for r in warnings)
