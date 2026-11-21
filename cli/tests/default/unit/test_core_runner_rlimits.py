import resource

import pytest

from semgrep import core_runner


def tried_soft_limits(monkeypatch, soft, hard, refuse_above=None):
    tried = []

    def setrlimit(which, limits):
        tried.append(limits)
        if refuse_above is not None and limits[0] > refuse_above:
            raise ValueError("not allowed")

    monkeypatch.setattr(core_runner, "IS_WINDOWS", False)
    monkeypatch.setattr(core_runner.resource, "getrlimit", lambda which: (soft, hard))
    monkeypatch.setattr(core_runner.resource, "setrlimit", setrlimit)
    core_runner.setrlimits_preexec_fn()
    return tried


@pytest.mark.quick
def test_unlimited_hard_limit_is_not_divided(monkeypatch):
    # Python 3.15 reports an unlimited limit as 2**64 - 1 instead of -1
    tried = tried_soft_limits(monkeypatch, 8388608, resource.RLIM_INFINITY)
    assert tried == [(1000000000, resource.RLIM_INFINITY)]


@pytest.mark.quick
def test_finite_hard_limit_still_tries_fractions_first(monkeypatch):
    hard = 3 * 1000000000 * 10
    tried = tried_soft_limits(monkeypatch, 8388608, hard)
    assert tried == [(int(hard / 3), hard)]


@pytest.mark.quick
def test_unlimited_soft_limit_is_kept_as_last_resort(monkeypatch):
    inf = resource.RLIM_INFINITY
    tried = tried_soft_limits(monkeypatch, inf, inf, refuse_above=0)
    assert all(
        soft in (1000000000, 512000000, 51200000, 5120000, inf) for soft, _ in tried
    )
