import multiprocessing
import subprocess
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Optional

from semgrep import __VERSION__
from semgrep.app.scans import ScanHandler
from semgrep.constants import DEFAULT_MAX_MEMORY_PRO_CI
from semgrep.constants import DEFAULT_PRO_TIMEOUT_CI
from semgrep.error import SemgrepError
from semgrep.meta import GitMeta
from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_interfaces import semgrep_output_v1 as out
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class EngineType(Enum):
    OSS = auto()
    # Secrets is abusing the fact that the runner code currently
    # invoke semgrep_core_proprietary when PRO_LANG is the engine type
    # instead of adding another engine type, because secrets like the
    # pro languages is really orthogonal, but implement in
    # semgrep-core-proprietary.
    PRO_LANG = auto()
    PRO_INTRAFILE = auto()
    PRO_INTERFILE = auto()

    @classmethod
    def decide_engine_type(
        cls,
        requested_engine: Optional["EngineType"] = None,
        scan_handler: Optional[ScanHandler] = None,
        git_meta: Optional[GitMeta] = None,
        run_secrets: bool = False,
        enable_pro_diff_scan: bool = False,
        supply_chain_only: bool = False,
    ) -> "EngineType":
        """Select which Semgrep engine type to use if none is explicitly requested.

        Considers settings from Semgrep Cloud Platform and version control state.
        """
        # Change default to pro-engine intrafile if secrets was requested.
        # Secrets is built into pro-engine, but any pro-setting should work.
        if (
            not (scan_handler and scan_handler.deepsemgrep)
            and requested_engine is None
            and run_secrets
        ):
            requested_engine = cls.PRO_LANG
        elif run_secrets and requested_engine is cls.OSS:
            # Should be impossible if the CLI gates impossible arguement combinations.
            raise SemgrepError("Semgrep Secrets is not part of the open source engine")

        if git_meta and scan_handler:
            if scan_handler.deepsemgrep and requested_engine is None:
                requested_engine = cls.PRO_INTERFILE

            if (
                requested_engine == cls.PRO_INTERFILE
                and not git_meta.is_full_scan
                and not enable_pro_diff_scan
            ):
                requested_engine = cls.PRO_INTRAFILE

        # Using PRO_LANG engine since PRO_INTERFILE/PRO_INTRAFILE defaults to -j 1
        # note if using OSS, then will keep using OSS
        if (
            requested_engine in {cls.PRO_INTERFILE, cls.PRO_INTRAFILE}
            and supply_chain_only
        ):
            logger.info(
                "Running only supply chain rules so running without extra interfile analysis"
            )
            return cls.PRO_LANG

        return requested_engine or cls.OSS

    def get_pro_version(self) -> str:
        binary_path = self.get_binary_path()
        if not binary_path:
            return "N/A"
        output: str = sub_check_output(
            [binary_path, "-pro_version"],
            timeout=10,
            encoding="utf-8",
            stderr=subprocess.STDOUT,
        )
        return output.rstrip()

    @staticmethod
    def get_cpu_count() -> int:
        try:
            return multiprocessing.cpu_count()
        except NotImplementedError:  # on Windows
            return 1

    @property
    def default_jobs(self) -> int:
        if self == EngineType.PRO_INTERFILE:
            return 1
        # Maxing out number of cores used to 16 if more not requested to not overload on large machines
        return min(16, self.get_cpu_count())

    @property
    def default_max_memory(self) -> int:
        """Interfile uses a lot of memory, so it should have a safe default limit."""
        if self == EngineType.PRO_INTERFILE:
            return DEFAULT_MAX_MEMORY_PRO_CI
        return 0  # unlimited

    @property
    def default_interfile_timeout(self) -> int:
        """Interfile uses a lot of time, so it should have a safe default limit."""
        if self == EngineType.PRO_INTERFILE:
            return DEFAULT_PRO_TIMEOUT_CI
        return 0  # unlimited

    def get_binary_path(self) -> Optional[Path]:
        if self.is_pro:
            if self.check_is_correct_pro_version():
                return SemgrepCore.pro_path()
            else:
                return None
        else:
            return SemgrepCore.path()

    # Checks the version stamp that is installed alongside the
    # semgrep-core-proprietary binary to ensure that semgrep-core-proprietary
    # was installed by the current version of Semgrep.
    #
    # See also commands/install.py add_semgrep_pro_version_stamp.
    def check_is_correct_pro_version(self) -> bool:
        version_stamp_path = SemgrepCore.pro_version_stamp_path()
        if version_stamp_path.is_file():
            with version_stamp_path.open("r") as f:
                version_at_install = f.readline().strip()
                return version_at_install == __VERSION__
        else:
            return False

    def check_if_installed(self) -> bool:
        binary_path = self.get_binary_path()
        return binary_path is not None and binary_path.exists()

    @property
    def has_dataflow_traces(self) -> bool:
        return self in {EngineType.PRO_INTRAFILE, EngineType.PRO_INTERFILE}

    @property
    def is_pro(self) -> bool:
        return self.value >= EngineType.PRO_LANG.value

    @property
    def is_interfile(self) -> bool:
        return self == EngineType.PRO_INTERFILE

    def to_engine_kind(self) -> out.EngineKind:
        if self.value == EngineType.OSS.value:
            return out.EngineKind(out.OSS())
        else:
            return out.EngineKind(out.PRO())
