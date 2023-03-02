from __future__ import annotations

from pathlib import Path
from typing import Protocol
from typing import TYPE_CHECKING

from semgrep.constants import OutputFormat

if TYPE_CHECKING:
    from tests.conftest import SemgrepResult


class RunSemgrep(Protocol):
    def __call__(
        # if you change these args, mypy will require updating tests.conftest._run_semgrep too
        self,
        config: str | Path | list[str] | None = None,
        *,
        target_name: str | None = "basic",
        options: list[str | Path] | None = None,
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
    ) -> SemgrepResult:
        ...
