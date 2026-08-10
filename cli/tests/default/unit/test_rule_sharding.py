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
import pytest

from semgrep.core_runner import _shard_count
from semgrep.core_runner import _shard_rules
from semgrep.core_runner import RULE_FILE_SHARD_MAX_COUNT
from semgrep.core_runner import RULE_FILE_SHARD_MIN_RULES
from semgrep.core_runner import RULE_FILE_SHARD_TARGET_BYTES

MiB = 1024 * 1024


class TestShardCount:
    @pytest.mark.quick
    @pytest.mark.parametrize("jobs", [None, 1, 8])
    def test_small_rulesets_are_not_sharded(self, jobs):
        """Below the floor, and within one shard's budget, stay single-file."""
        rules = RULE_FILE_SHARD_MIN_RULES - 1
        assert _shard_count(rules, jobs, RULE_FILE_SHARD_TARGET_BYTES) == 1

    @pytest.mark.quick
    def test_few_but_enormous_rules_are_still_sharded(self):
        """The floor counts rules; the pathology it guards against is bytes.

        A handful of giant rules must not slip past the byte budget just
        because there are not many of them.
        """
        rules = RULE_FILE_SHARD_MIN_RULES - 1
        assert _shard_count(rules, None, 500 * MiB) == rules
        assert _shard_count(10, None, 500 * MiB) == 10

    @pytest.mark.quick
    def test_explicit_single_job_is_not_sharded(self):
        """`--jobs 1` means one domain in core, where sharding only adds cost."""
        assert _shard_count(100_000, 1, 500 * MiB) == 1

    @pytest.mark.quick
    def test_shards_without_explicit_jobs(self):
        """No `--jobs` means core picks its own domain count, so still shard.

        This is the default configuration for `semgrep scan` and `semgrep ci`,
        and the case the old jobs-derived policy left entirely unsharded.
        """
        assert _shard_count(10_240, None, 114 * MiB) == 57

    @pytest.mark.quick
    def test_scales_with_bytes_not_jobs(self):
        """Doubling the ruleset doubles the shards; doubling jobs does not."""
        assert _shard_count(10_240, 8, 64 * MiB) == 32
        assert _shard_count(10_240, 8, 128 * MiB) == 64
        assert _shard_count(10_240, 16, 64 * MiB) == 32

    @pytest.mark.quick
    def test_jobs_is_a_floor_so_small_rulesets_never_regress(self):
        """A ruleset under one shard-budget per worker keeps today's count."""
        assert _shard_count(10_000, 8, 1 * MiB) == 8
        assert _shard_count(10_000, 32, 1 * MiB) == 32

    @pytest.mark.quick
    def test_never_more_shards_than_rules(self):
        assert _shard_count(200, 8, 500 * MiB) == 200

    @pytest.mark.quick
    def test_backstop_caps_pathological_rulesets(self):
        huge = RULE_FILE_SHARD_TARGET_BYTES * RULE_FILE_SHARD_MAX_COUNT * 4
        assert _shard_count(10**6, 8, huge) == RULE_FILE_SHARD_MAX_COUNT

    @pytest.mark.quick
    def test_evenly_sized_rules_land_under_the_budget(self):
        """Composed with _shard_rules, the largest real shard meets the budget.

        Only holds when no single rule is outsized -- see
        test_budget_cannot_bound_an_outsized_rule.
        """
        sizes = [4096] * 100_000
        count = _shard_count(len(sizes), 8, sum(sizes))
        largest = max(
            sum(sizes[i] for i in shard) for shard in _shard_rules(sizes, count)
        )
        assert largest <= RULE_FILE_SHARD_TARGET_BYTES

    @pytest.mark.quick
    def test_budget_cannot_bound_an_outsized_rule(self):
        """A shard is never smaller than its largest rule.

        Pins the known limit of the byte budget so it is not mistaken for a
        hard bound: one huge rule overshoots the target however many shards
        it is spread across.
        """
        sizes = [50 * MiB] + [1000] * 300
        count = _shard_count(len(sizes), 8, sum(sizes))
        largest = max(
            sum(sizes[i] for i in shard) for shard in _shard_rules(sizes, count)
        )
        assert largest >= 50 * MiB


class TestShardRules:
    @pytest.mark.quick
    def test_single_shard_keeps_every_rule_in_order(self):
        assert _shard_rules([5, 1, 9], 1) == [[0, 1, 2]]

    @pytest.mark.quick
    @pytest.mark.parametrize("shard_count", [1, 2, 3, 7, 64])
    def test_partitions_every_rule_exactly_once(self, shard_count):
        sizes = [(i * 37) % 900 + 100 for i in range(500)]
        shards = _shard_rules(sizes, shard_count)
        assert len(shards) == shard_count
        assert sorted(i for shard in shards for i in shard) == list(range(500))

    @pytest.mark.quick
    def test_rules_keep_their_relative_order_within_a_shard(self):
        sizes = [(i * 7919) % 1000 for i in range(200)]
        for shard in _shard_rules(sizes, 8):
            assert shard == sorted(shard)

    @pytest.mark.quick
    def test_balances_by_bytes_not_by_count(self):
        """One huge rule and many small ones must not all land together."""
        sizes = [1000] + [1] * 99
        shards = _shard_rules(sizes, 2)
        totals = sorted(sum(sizes[i] for i in shard) for shard in shards)
        # Perfect byte balance is impossible here, but the 99 small rules
        # should all end up opposite the huge one rather than beside it.
        assert totals == [99, 1000]

    @pytest.mark.quick
    def test_beats_contiguous_slicing_when_heavy_rules_cluster(self):
        """Heavy rules cluster in real rulesets; contiguous slices unbalance.

        The parse phase finishes no sooner than its largest shard, so what
        matters is the maximum, not the mean.
        """
        # First quarter of the ruleset is 10x heavier than the rest.
        sizes = [10_000] * 100 + [1_000] * 300
        shard_count = 8

        balanced = max(
            sum(sizes[i] for i in shard) for shard in _shard_rules(sizes, shard_count)
        )
        per_shard = len(sizes) // shard_count
        contiguous = max(
            sum(sizes[i : i + per_shard]) for i in range(0, len(sizes), per_shard)
        )
        assert balanced * 2 < contiguous

    @pytest.mark.quick
    def test_ties_are_broken_by_index(self):
        """All-equal sizes are the case where tie-breaking decides everything.

        Asserting the exact partition is what catches a lost tie-break; two
        calls agreeing would pass even without one.
        """
        assert _shard_rules([100] * 8, 4) == [[0, 4], [1, 5], [2, 6], [3, 7]]

    @pytest.mark.quick
    def test_handles_more_shards_than_rules(self):
        """_shard_count avoids this, but empty shards must not corrupt output."""
        shards = _shard_rules([1, 2, 3], 5)
        assert len(shards) == 5
        assert sorted(i for shard in shards for i in shard) == [0, 1, 2]
