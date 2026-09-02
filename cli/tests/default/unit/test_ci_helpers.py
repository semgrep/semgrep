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

from semgrep.commands.ci import _filter_app_ignored_matches


class LinearMembershipForbiddenList(list[str]):
    def __contains__(self, item: object) -> bool:
        raise AssertionError("ignored ID lists must not be searched linearly")


@pytest.mark.quick
def test_filter_app_ignored_matches_uses_set_membership(mocker):
    rule = mocker.sentinel.rule
    kept = mocker.Mock(syntactic_id="keep-sid", match_based_id="keep-mid")
    syntactically_ignored = mocker.Mock(
        syntactic_id="ignored-sid", match_based_id="keep-mid"
    )
    match_based_ignored = mocker.Mock(
        syntactic_id="keep-sid", match_based_id="ignored-mid"
    )

    result = _filter_app_ignored_matches(
        {rule: [kept, syntactically_ignored, match_based_ignored]},
        LinearMembershipForbiddenList(["ignored-sid"]),
        LinearMembershipForbiddenList(["ignored-mid"]),
    )

    assert result == {rule: [kept]}
