import sys
from pathlib import Path
from typing import List


def read_timing(filename: Path) -> List[float]:
    print(f"Reading {filename}")
    times = filename.read_text()
    return [float(t) for t in times.split()]


def main() -> None:
    baseline_timing_file = Path(sys.argv[1])
    baseline_times = read_timing(baseline_timing_file)

    latest_timing_file = Path(sys.argv[2])
    latest_times = read_timing(latest_timing_file)

    for baseline_time, latest_time in zip(baseline_times, latest_times):
        print(f"Baseline: {baseline_time}, Latest: {latest_time}")

        # Assert latest time is not more than 6% slower than baseline
        assert latest_time < baseline_time * 1.06


if __name__ == "__main__":
    main()
