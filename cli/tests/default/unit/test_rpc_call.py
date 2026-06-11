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

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.rpc_call import get_targets


class TestGetTargets:
    @pytest.mark.quick
    def test_raises_on_rpc_failure(self, monkeypatch):
        """When rpc_call returns None (semgrep-core crash/failure),
        get_targets should raise SemgrepError instead of silently
        returning an empty result."""
        monkeypatch.setattr("semgrep.rpc_call.rpc_call", lambda *args, **kwargs: None)

        scanning_roots = out.ScanningRoots(
            root_paths=[],
            targeting_conf=out.TargetingConf(
                exclude=[],
                max_target_bytes=0,
                respect_gitignore=True,
                respect_semgrepignore_files=True,
                always_select_explicit_targets=False,
                explicit_targets=[],
                force_novcs_project=False,
                exclude_minified_files=False,
                exclude_binary_files=False,
            ),
        )

        with pytest.raises(SemgrepError, match="Failed to obtain target files"):
            get_targets(scanning_roots)
