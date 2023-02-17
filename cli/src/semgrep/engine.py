import multiprocessing
import subprocess
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Optional

from semgrep.app.scans import ScanHandler
from semgrep.meta import GitMeta
from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_interfaces import semgrep_output_v1 as out
from semgrep.util import sub_check_output


class EngineType(Enum):
    OSS = auto()
    PRO_LANG = auto()
    PRO_INTRAFILE = auto()
    PRO_INTERFILE = auto()

    @classmethod
    def decide_engine_type(
        cls,
        requested_engine: Optional["EngineType"] = None,
        scan_handler: Optional[ScanHandler] = None,
        git_meta: Optional[GitMeta] = None,
    ) -> "EngineType":
        """Select which Semgrep engine type to use if none is explicitly requested.

        Considers settings from Semgrep Cloud Platform and version control state.
        """
        if git_meta and scan_handler:
            if requested_engine == cls.PRO_INTERFILE and not git_meta.is_full_scan:
                requested_engine = cls.PRO_LANG

            if scan_handler.deepsemgrep:
                return cls.PRO_INTERFILE if git_meta.is_full_scan else cls.PRO_LANG

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
        return self.get_cpu_count()

    def get_binary_path(self) -> Optional[Path]:
        return SemgrepCore.pro_path() if self.is_pro else SemgrepCore.path()

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
