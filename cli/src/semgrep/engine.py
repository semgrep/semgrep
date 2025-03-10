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
from semgrep.util import sub_check_output, IS_WINDOWS
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
        logged_in: bool = False,
        engine_flag: Optional["EngineType"] = None,
        run_secrets: bool = False,
        interfile_diff_scan_enabled: bool = False,
        # ci-only args
        ci_scan_handler: Optional[ScanHandler] = None,
        git_meta: Optional[GitMeta] = None,
        supply_chain_only: bool = False,
    ) -> "EngineType":
        """Determine which Semgrep engine type to run with.

        Takes into account the following rules:
        - The CLI flag > Semgrep Cloud Platform settings > defaults
        - Requesting the secrets engine implies requesting PRO_INTRAFILE
        - By default, logged in `semgrep ci` scans use PRO_INTRAFILE
        - By default, all other scans use OSS

        There are also some restrictions based on version control state and
        product requested, to ensure users get fast scans when they expect it.
        """
        interfile_is_requested_via_app = ci_scan_handler and ci_scan_handler.deepsemgrep

        if engine_flag is not None:
            requested_engine = engine_flag
        elif interfile_is_requested_via_app:
            requested_engine = cls.PRO_INTERFILE
        elif run_secrets:
            requested_engine = cls.PRO_INTRAFILE
        elif logged_in and ci_scan_handler:
            # - logged_in indicates that pro analysis is available to the user
            # - ci_scan_handler indicates that `semgrep ci` was the entrypoint
            # Given these two conditions, the default engine is PRO_INTRAFILE
            # Note: `ci_scan_handler` currently requires being logged in, but
            # we check both explicitly in case that changes
            requested_engine = cls.PRO_INTRAFILE
        else:
            requested_engine = cls.OSS

        # Override 1: diff scans should run with PRO_INTRAFILE when PRO_INTERFILE
        # is requested. This ensures diff scans remain fast.
        # TODO we can delete this once interfile diff scans are GA

        diff_scan = git_meta and not git_meta.is_full_scan
        if (
            diff_scan
            and not interfile_diff_scan_enabled
            and requested_engine is cls.PRO_INTERFILE
        ):
            requested_engine = cls.PRO_INTRAFILE

        # Override 2: Turn off PRO_INTERFILE when only supply chain is requested
        # This is necessary because PRO_INTERFILE defaults to `-j 1`
        if supply_chain_only and requested_engine is cls.PRO_INTERFILE:
            requested_engine = cls.PRO_INTRAFILE

        cls.validate_requested_engine(run_secrets, requested_engine)

        return requested_engine

    @staticmethod
    def validate_requested_engine(
        run_secrets: bool, requested_engine: "EngineType"
    ) -> None:
        """Sanity check that the requested engine is compatible with the product

        TODO Check if we need this step and remove it if we decide it's redundant
        """
        # TODO: if we keep this step, should we also fail here if logged_in is false
        # and requested_engine is not cls.OSS?
        # This would no longer allow people to run the pro engine without log in
        # if they acquire a copy of the binary (either through us or not)

        if run_secrets and requested_engine is EngineType.OSS:
            # Should be impossible if the CLI gates impossible argument combinations.
            raise SemgrepError("Semgrep Secrets is not part of the open source engine")

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
        # Windows is currently limited to 1 job
        if self == EngineType.PRO_INTERFILE or IS_WINDOWS:
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
            return out.EngineKind(out.OSS_())
        else:
            return out.EngineKind(out.PRO_())
