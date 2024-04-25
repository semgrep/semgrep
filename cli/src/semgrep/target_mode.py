"""
Target mode configuration for Semgrep Scan

It's important to note that the targeting schemes differ among three scanning
modes: pro (interfile) differential scanning, open-source (per-file)
differential scanning, and whole scan. Here's a breakdown of their respective
targeting requirements:

Pro Diff Scan: In this mode, all input files are designated as "targets", and
specifically, the files that have changed between the head and baseline commits
are considered "diff targets".

OSS Diff Scan: For OSS differential scanning, the files that have changed
between the head and baseline commits serve as the primary "targets".

Whole Scan: In the case of the whole scan, all input files are categorized as
"targets" without any distinction based on commit changes.
"""
from pathlib import Path
from typing import FrozenSet
from typing import Union

from attr import field
from attr import frozen


@frozen
class WholeScan:
    ()


@frozen
class HistoricalScan:
    ()


@frozen
class DiffScan:
    ()


@frozen
class ProDiffScan:
    # TODO: rename this class to InterfileDiffScan
    diff_targets: FrozenSet[Path] = field()
    diff_depth: int = field()


@frozen
class TargetModeConfig:
    scan_type: Union[WholeScan, HistoricalScan, DiffScan, ProDiffScan] = field()

    @classmethod
    def pro_diff_scan(
        cls, diff_targets: FrozenSet[Path], diff_depth: int
    ) -> "TargetModeConfig":
        return cls(ProDiffScan(diff_targets, diff_depth))

    @classmethod
    def whole_scan(cls) -> "TargetModeConfig":
        return cls(WholeScan())

    @classmethod
    def historical_scan(cls) -> "TargetModeConfig":
        return cls(HistoricalScan())

    @classmethod
    def diff_scan(cls) -> "TargetModeConfig":
        return cls(DiffScan())

    @property
    def is_historical_scan(self) -> bool:
        return isinstance(self.scan_type, HistoricalScan)

    @property
    def is_pro_diff_scan(self) -> bool:
        return isinstance(self.scan_type, ProDiffScan)

    @property
    def is_diff_scan(self) -> bool:
        return isinstance(self.scan_type, DiffScan)

    def get_diff_targets(self) -> FrozenSet[Path]:
        if isinstance(self.scan_type, WholeScan) or isinstance(
            self.scan_type, DiffScan
        ):
            raise ValueError("not a pro diff scan")
        elif isinstance(self.scan_type, ProDiffScan):
            return self.scan_type.diff_targets
        else:
            raise ValueError("unknown scan type: " + str(self.scan_type))

    def get_diff_depth(self) -> int:
        if isinstance(self.scan_type, WholeScan) or isinstance(
            self.scan_type, DiffScan
        ):
            raise ValueError("not a pro diff scan")
        elif isinstance(self.scan_type, ProDiffScan):
            return self.scan_type.diff_depth
        else:
            raise ValueError("unknown scan type: " + str(self.scan_type))
