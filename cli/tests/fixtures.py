# Type definitions used in tests
#
# These definitions are presumably not placed in 'conftest.py' so they can be
# shared across semgrep and semgrep-proprietary, each having their own
# 'conftest.py'.
# TODO: confirm the assumption above
#
from __future__ import annotations

from pathlib import Path
from typing import Callable
from typing import Optional
from typing import Protocol
from typing import TYPE_CHECKING

from semgrep.constants import OutputFormat


# TYPE_CHECKING is a constant that ensures some imports (e.g., from
# tests.conftest) are only processed during type checking (e.g., by
# mypy), and not at runtime.
#
# This is presumably what allows for cyclic dependency between the two
# modules for typechecking purposes.
# TODO: simplify!
#
if TYPE_CHECKING:
    from tests.conftest import SemgrepResult


# The type of run_semgrep functions defined in a project's 'conftest.py'.
# Run 'make typecheck' to run mypy on the project.
#
class RunSemgrep(Protocol):
    def __call__(
        self,
        config: str | Path | list[str] | None = None,
        *,
        subcommand: str | None = None,
        target_name: str | None = "basic",
        options: Optional[list[str]] = None,
        output_format: OutputFormat | None = OutputFormat.JSON,
        strict: bool = True,
        quiet: bool = False,
        env: dict[str, str] | None = None,
        assert_exit_code: None | int | set[int] = 0,
        force_color: bool | None = None,
        assume_targets_dir: bool = True,
        force_metrics_off: bool = True,
        stdin: str | None = None,
        clean_fingerprint: bool = True,
        use_click_runner: bool = False,
        prepare_workspace: Callable[[], None] = lambda: None,
        teardown_workspace: Callable[[], None] = lambda: None,
    ) -> SemgrepResult:
        ...
