# fork_subprocess.py
"""
Define the fork_subprocess.Process class, which uses the
'multiprocessing' library to fork a child process, then expose its
stdout and stderr as 'asyncio.StreamReaders' in the parent.
"""
import asyncio  # StreamReader, gather, run
import multiprocessing  # Process
import os  # fdopen, pipe, dup2, etc.
from typing import Callable


# This is a copy of asyncio.subprocess._DEFAULT_LIMIT.
DEFAULT_LIMIT = 2**16  # 64 KiB


# Related SO Q+A:
#   https://stackoverflow.com/questions/52089869/how-to-create-asyncio-stream-reader-writer-for-stdin-stdout
#   https://stackoverflow.com/questions/64303607/python-asyncio-how-to-read-stdin-and-write-to-stdout
async def reader_for_pipe_fd(fd: int, limit: int) -> asyncio.StreamReader:
    """
    Return a StreamReader for pipe file descriptor 'fd'.

    The 'limit' defines the maximum size of a single 'read' operation to
    avoid the possibility of deadlock.
    """
    # Get the running loop (which must exist), and pass it to the new
    # objects so everyone is using the same loop.
    loop = asyncio.get_running_loop()

    # Build the reader to return.  StreamReader exposes an asynchronous
    # interface for reading.  It requires that something synchronously
    # feed it data that it can provide to the client.
    #
    # I do not know what 'limit' really represents (the documentation is
    # vague).  It is used by some sketchy-looking logic inside
    # StreamReader; I suspect a latent bug.  This function's
    # documentation contains my empirical observation.
    reader = asyncio.StreamReader(loop=loop, limit=limit)

    # Wrap a Python file object around 'fd'.  It does not own 'fd';
    # the client will continue to own it.  This file object does not get
    # explicitly closed.
    #
    # Internally, 'connect_read_pipe' basically just calls 'fileno()' to
    # get the descriptor back out, and insists it is a pipe or socket.
    # It does not work with arbitrary files.
    fd_file = os.fdopen(fd, "rb", closefd=False)

    # Add 'fd_file' to the set of descriptors monitored by 'loop' (and
    # remove it upon EOF).
    #
    # Although 'connect_read_pipe' is 'async', it does not really block;
    # it will succeed or fail immediately, although notification takes a
    # detour through the 'call_soon' queue.
    await loop.connect_read_pipe(
        # Build the "protocol" object.  The protocol object has
        # synchronous callback methods that are invoked when the
        # underlying file descriptor changes state (is closed, has data,
        # etc.).  This one informs 'reader' when that happens, allowing
        # it to wake up a coroutine awaiting its data.
        #
        # StreamReaderProtocol was documented in Python 3.6 but was then
        # un-documented in Python 3.7 despite not being deprecated.  I
        # don't know why, nor am I aware of a direct replacement.
        lambda: asyncio.StreamReaderProtocol(reader, loop=loop),
        fd_file,
    )

    return reader


# There is a Pipe class in multiprocessing, but that provides a
# message-oriented interface, whereas I want access to the raw byte
# stream of the 'read' and 'write' system calls.
class Pipe:
    """
    Pair of file descriptors created by 'os.pipe()'.
    """

    def __init__(self) -> None:
        (self.read_fd, self.write_fd) = os.pipe()

    def set_write_inheritable(self) -> None:
        os.set_inheritable(self.write_fd, True)

    def close_write_end(self) -> None:
        os.close(self.write_fd)

    def close_read_end(self) -> None:
        os.close(self.read_fd)


class Process:
    """
    This class manages a subprocess created by 'os.fork()', and has an
    interface somewhat similar to asyncio.subprocess.Process,
    particularly exposing stdout and stderr as StreamReaders.
    """

    # This class could fairly easily also provide a StreamWriter for
    # communicating with the child's stdin, but I've chosen not to do so
    # simply because I do not need that capability right now.

    def __init__(
        self,
        func: Callable[[], None],
        stdout_pipe: Pipe,
        stderr_pipe: Pipe,
        stdout: asyncio.StreamReader,
        stderr: asyncio.StreamReader,
    ) -> None:
        """
        This constructor is intended to be private to this module.  Use
        'start_fork_subprocess()' to make an object.
        """
        # Function to invoke to perform the child's logic.
        self._func = func

        self._stdout_pipe = stdout_pipe
        self._stderr_pipe = stderr_pipe

        # Public access to the streams of child output.
        self.stdout = stdout
        self.stderr = stderr

        # Arguments to pass to '_callFunc'.
        writeFds = (self._stdout_pipe.write_fd, self._stderr_pipe.write_fd)

        # Get a "context" object so I can insist on using the fork
        # method (which is not available on Windows).
        mpctx = multiprocessing.get_context("fork")

        # Dirty hack to avoid the assertion failure:
        #
        #   "daemonic processes are not allowed to have children"
        #
        # Without this line, if fork_subprocess is used within a process
        # that was itself started by multiprocess, such as what happens
        # when Pool is used by semgrep/test.py, the quoted assertion
        # fails.
        #
        # My understanding is the purpose of the assertion is to catch
        # cases where someone creates pools within pools, at each level
        # using all cores, and thereby spawning an unreasonable number
        # of processes.  But my use case is to spawn exactly one child,
        # while the parent merely monitors the child's output, so the
        # risk of inadvertently creating a "fork bomb" is absent.
        #
        # From googling the error, there seem to be a wide variety of
        # suggested ways to work around the check, but no official
        # method built in to the multiprocessing library.  Since this is
        # simple and seems to work, I'm going with it.
        multiprocessing.process.current_process().daemon = False

        # Fork the child process.
        self._child = mpctx.Process(target=self._callFunc, args=writeFds)
        self._child.start()

    def _callFunc(self, outfd: int, errfd: int) -> None:
        """
        Invoke '_func(_arg)' after redirecting stdout and stderr.
        """
        # Have the child write its stdout to 'outfd'.
        os.dup2(outfd, 1)

        # Now close 'outfd', as we will exclusively use stdout.
        os.close(outfd)

        # Do the same for stderr.
        os.dup2(errfd, 2)
        os.close(errfd)

        # Now run the main child function.
        self._func()

    def wait(self) -> int:
        """
        Synchronously wait for the child process to terminate, and
        return its exit code.
        """
        self._child.join()

        # Close the read end of the pipes.
        self._stdout_pipe.close_read_end()
        self._stderr_pipe.close_read_end()

        assert self._child.exitcode is not None
        return self._child.exitcode


async def start_fork_subprocess(
    func: Callable[[], None], limit: int = DEFAULT_LIMIT
) -> Process:
    """
    Fork a child process that executes 'func()'.

    Return the Process object that manages the child.
    """
    # Create pipes for communication between parent and child.  The
    # child will write, the parent will read.
    stdout_pipe = Pipe()
    stderr_pipe = Pipe()

    # Allow the child to inherit the write ends.
    stdout_pipe.set_write_inheritable()
    stderr_pipe.set_write_inheritable()

    # Constructors cannot be async, so the readers have to be made
    # first.
    stdout = await reader_for_pipe_fd(stdout_pipe.read_fd, limit)
    stderr = await reader_for_pipe_fd(stderr_pipe.read_fd, limit)

    # Package the instance data in an object and actually start
    # the child process.
    ret = Process(func, stdout_pipe, stderr_pipe, stdout, stderr)

    # In the parent, close the write end of the pipes.
    stdout_pipe.close_write_end()
    stderr_pipe.close_write_end()

    return ret


# EOF
