#!/usr/bin/env python3
# test_fork_subprocess.py
"""
Demonstrate the 'fork_subprocess' module.
"""
import asyncio  # StreamReader
import os  # getpid
import sys  # exit, stderr
import time  # sleep
import traceback  # print_tb
from typing import Any
from typing import List

import pytest  # CaptureFixture, mark

import semgrep.fork_subprocess as fork_subprocess


# When true, suppress printing in 'prinq'.
g_quiet = False


def set_quiet(b: bool) -> None:
    """Set 'g_quiet'."""
    # nosemgrep
    global g_quiet
    g_quiet = b


def prinq(*args: Any, **kwargs: Any) -> None:
    """Same as 'print', unless 'g_quiet'."""
    if not g_quiet:
        print(*args, **kwargs)


def info(label: str) -> None:
    """Print PIDs, etc."""

    prinq(f"{label}: module name:", __name__)
    prinq(f"{label}: parent process:", os.getppid())
    prinq(f"{label}: process id:", os.getpid())


def child(argv: List[str]) -> None:
    """Function to run in the child process."""

    # In the child, we need to communicate via stdout/err.
    set_quiet(False)

    info("child")

    prinq(f"argv: {argv}")
    prinq("output 1 by child")
    prinq("errput 1 by child", file=sys.stderr)
    time.sleep(0.05)
    prinq("output 2 by child")
    prinq("errput 2 by child", file=sys.stderr)
    prinq("child exiting with code 5")

    sys.exit(5)


async def consume_reader(label: str, reader: asyncio.StreamReader) -> None:
    """
    Print all the data written to 'reader', prefixed with 'label'.
    """
    all: bytes = b""
    while True:
        data = await reader.read(10)
        if len(data) == 0:
            prinq(f"parent: {label}: received empty byte array")
            break
        prinq(f"parent: {label}: received: {data!r}")
        all += data

    output: str = all.decode("UTF-8")
    lines: List[str] = output.splitlines()
    prinq(f"parent: all of {label}:")
    for line in lines:
        prinq(f"  {line}")

    # Verify the data is as expected.
    if label == "stdout":
        assert output.startswith("child: module name")
        assert output.endswith("code 5\n")
    elif label == "stderr":
        assert output == "errput 1 by child\nerrput 2 by child\n"
    else:
        raise ValueError(f"unexpected label: {label}")


async def async_main() -> fork_subprocess.Process:
    """
    Asynchronous core of main().
    """
    proc = await fork_subprocess.start_fork_subprocess(lambda: child(["bob"]))

    results = await asyncio.gather(
        consume_reader("stdout", proc.stdout),
        consume_reader("stderr", proc.stderr),
        return_exceptions=True,
    )

    for r in results:
        if isinstance(r, Exception):
            traceback.print_tb(r.__traceback__)
            prinq(f"Exn processing output streams: {r!r}", file=sys.stderr)
            sys.exit(2)

    return proc


def main() -> None:
    """Main test."""
    info("parent")

    p = asyncio.run(async_main())

    # Wait for the child to terminate.
    code = p.wait()
    prinq(f"child exit code: {code}")


@pytest.mark.quick
def test_from_pytest(capsys: pytest.CaptureFixture[str]) -> None:
    """Test function as invoked by pytest."""

    # By default, pytest intercepts stdout/err, but that interferes with
    # the communication mechanism being tested, so disable it here.
    with capsys.disabled():
        # Disable the routine printouts made by the parent process so as
        # not to clutter the pytest output, given that we have disabled
        # the usual output capture.
        set_quiet(True)

        main()


if __name__ == "__main__":
    main()


# EOF
