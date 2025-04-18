"""
Target mode configuration for Semgrep Scan

It's important to note that the targeting schemes differ among three scanning
modes: pro (interfile) differential scanning, open-source (per-file)
differential scanning, and whole scan. Here's a breakdown of their respective
targeting requirements:

OSS Diff Scan: For OSS differential scanning, the files that have changed
between the head and baseline commits serve as the primary "targets".

Whole Scan: In the case of the whole scan, all input files are categorized as
"targets" without any distinction based on commit changes.
"""
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
class TargetModeConfig:
    scan_type: Union[WholeScan, HistoricalScan, DiffScan] = field()

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
    def is_diff_scan(self) -> bool:
        return isinstance(self.scan_type, DiffScan)
