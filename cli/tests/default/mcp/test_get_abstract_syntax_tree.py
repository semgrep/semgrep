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
from unittest.mock import AsyncMock
from unittest.mock import patch

import pytest

from semgrep.mcp.server import get_abstract_syntax_tree


@pytest.mark.quick
@pytest.mark.asyncio
async def test_get_abstract_syntax_tree_uses_show_dump_ast():
    """
    Regression for https://github.com/semgrep/semgrep/issues/11645.

    Semgrep 1.146.0 removed the legacy `semgrep --experimental --dump-ast
    -l LANG --json FILE` CLI form. The MCP tool must now invoke the
    replacement `semgrep show dump-ast LANG FILE --json`.

    The `@with_tool_span` tracing decorator is intentionally bypassed via
    `__wrapped__` so this test exercises only the argument-list construction
    for the semgrep subprocess. Tracing behavior is out of scope here and
    has its own test surface.
    """
    # Bypass the `@with_tool_span(is_semgrep_scan=False)` tracing wrapper so
    # we exercise only the argument-list construction.
    undecorated = get_abstract_syntax_tree.__wrapped__
    with patch(
        "semgrep.mcp.server.run_semgrep_output",
        new_callable=AsyncMock,
    ) as mock_run:
        mock_run.return_value = "{}"
        await undecorated(ctx=None, code="x = 1\n", language="python")

    assert mock_run.await_count == 1
    args = mock_run.await_args.kwargs["args"]
    # Modern CLI surface.
    assert args[0] == "show"
    assert args[1] == "dump-ast"
    assert "python" in args
    assert "--json" in args
    # Legacy CLI surface must not leak back in.
    assert "--experimental" not in args
    assert "--dump-ast" not in args
    assert "-l" not in args
