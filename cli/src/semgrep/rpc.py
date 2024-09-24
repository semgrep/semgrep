##############################################################################
# Prelude
##############################################################################
# Allows function calls from Python into OCaml, to allow us to incrementally
# migrate pysemgrep functionality to osemgrep piece by piece.
#
# See `src/rpc/README.txt` from the repository root for more details.
# coupling: src/rpc/RPC.handle_call()
# coupling: semgrep_output_v1.atd which defines the CallXxx and RetXxx
import subprocess
from typing import IO
from typing import Optional
from typing import Type
from typing import TypeVar

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.semgrep_core import SemgrepCore
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

##############################################################################
# Constants
##############################################################################

# This is a typical system default, but let's not leave it up to chance.
ENCODING = "utf-8"

# After a call, how long we wait, in seconds, for the subprocess to exit cleanly
# before killing it. There's no real downside to having this be low, except that
# we are more likely to get an error in the log even if there's no real issue
# except that the system is overloaded or something like that.
#
# We do want to log if we hit this timeout, though, since it *could* be
# indicative of a real problem.
SUBPROC_TIMEOUT_S = 1

##############################################################################
# Helpers
##############################################################################


# Read `size` bytes from `io`. Returns fewer bytes if we hit EOF.
def _really_read(io: IO[str], size: int) -> str:
    # Operate on bytes, not str.
    out: bytes = b""
    while len(out) < size:
        # `read` may return fewer bytes than requested. In practice this is
        # unlikely, but we need to handle it.
        #
        # We could have used a BufferedReader which does this for us, but to do
        # so we'd need access to the `buffer` property of the input stream.
        # While it's documented in the (terrible) Python API docs for at least
        # some kinds of input streams, MyPy doesn't recognize it and it's not
        # clear to me (nmote) whether it is guaranteed to be present on the
        # streams provided by subprocess.Popen. So, to be on the safe side,
        # we'll just do this ourselves.
        new: str = io.read(size)
        # This happens if we hit EOF. In that case, repeatedly reading will lead
        # to an infinite loop.
        if len(new) == 0:
            logger.error(f"0 bytes read from RPC input stream")
            break
        out = out + new.encode(ENCODING)
    return out.decode(ENCODING)


def _read_packet(io: IO[str]) -> Optional[str]:
    # Unlike `read`, `readline` is guaranteed to return a full line unless there
    # is an EOF
    size_str = io.readline().strip()
    if not size_str.isdigit():
        # Avoid horrific log spew if we somehow got a really long line
        truncated = size_str[:50]
        logger.error(f"RPC input error: Expected a number, got '{truncated}'")
        return None
    size = int(size_str)
    return _really_read(io, size)


def _write_packet(io: IO[str], packet: str) -> None:
    # Size in bytes
    size: int = len(packet.encode(ENCODING))
    size_str = str(size) + "\n"
    io.write(size_str)
    io.write(packet)
    io.flush()


def _parse_function_return(packet: str) -> Optional[out.FunctionReturn]:
    try:
        return out.FunctionReturn.from_json_string(packet)
    # There are at least two kinds of exceptions that can be raised during
    # deserialization. Instead of enumerating them and hoping that we stay up to
    # date, let's just use a catch-all. In the end it doesn't really matter
    # exactly what went wrong, we still want to catch it and log.
    except Exception as e:
        truncated = packet[:50]
        logger.error(f"Error while deserializing RPC response '{truncated}': {e}")
        return None


##############################################################################
# Entry point
##############################################################################
T = TypeVar("T")


def rpc_call(call: out.FunctionCall, cls: Type[T]) -> Optional[T]:
    # We always use the pro binary if it's available. It's up to the caller to
    # appropriately handle the case where the pro function is not available and
    # to ensure that pro RPC methods are only called during a pro scan.
    semgrep_core_path = SemgrepCore.pro_path() or SemgrepCore.executable_path()
    with subprocess.Popen(
        [semgrep_core_path, "-rpc"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
        encoding=ENCODING,
    ) as proc:
        try:
            # These need to be local variables because otherwise mypy doesn't
            # trust the results of the None checks.
            proc_stdin = proc.stdin
            proc_stdout = proc.stdout
            if proc_stdin is None or proc_stdout is None:
                # This can't happen, since we set stdin and stdout args to PIPE
                # above. But mypy doesn't understand that. But log just in case
                # it actually can happen.
                logger.error(f"RPC subprocess missing stdout or stdin channel")
                return None
            call_str = call.to_json_string().strip()
            _write_packet(proc_stdin, call_str)
            proc_stdin.close()

            ret_str = _read_packet(proc_stdout)
            if ret_str is None:
                logger.error(f"Unable to read RPC response")
                return None
            ret = _parse_function_return(ret_str)
            if ret is None:
                # No need to log here, it's handled in the error case of
                # _parse_function_return
                return None
            # Any request can return an error
            if isinstance(ret.value, out.RetError):
                err: str = ret.value.value
                logger.error(f"RPC response indicated an error: {err}")
                return None
            # Check that we got the correct kind of response
            if isinstance(ret.value, cls):
                return ret.value
            else:
                logger.error(f"Received an incorrect kind of RPC response")
                return None
        finally:
            try:
                proc.wait(timeout=SUBPROC_TIMEOUT_S)
            except subprocess.TimeoutExpired:
                logger.error(f"RPC subprocess did not exit cleanly. Killing it.")
                proc.kill()
