import sys
from pathlib import Path
from typing import List


def read_timing(filename: Path) -> List[float]:
    print(f"Reading {filename}")
    times = filename.read_text()
    return [float(t) for t in times.split()]


def main() -> None:
    # There are two versions of each timing to improve stability

    baseline_timing_file_1 = Path(sys.argv[1])
    baseline_timing_file_2 = Path(sys.argv[2])
    baseline_times = zip(
        read_timing(baseline_timing_file_1), read_timing(baseline_timing_file_2)
    )
    baseline_times = [max(t1, t2) for t1, t2 in baseline_times]

    latest_timing_file_1 = Path(sys.argv[3])
    latest_timing_file_2 = Path(sys.argv[4])
    latest_times = zip(
        read_timing(latest_timing_file_1), read_timing(latest_timing_file_2)
    )
    latest_times = [min(t1, t2) for t1, t2 in latest_times]

    for baseline_time, latest_time in zip(baseline_times, latest_times):
        print(f"Baseline: {baseline_time}, Latest: {latest_time}")

        # Assert latest time is not more than 6% slower than baseline
        # or is within a fixed "probably environmental" range
        assert latest_time < baseline_time * 1.06 or latest_time - baseline_time < 0.5


if __name__ == "__main__":
    main()
