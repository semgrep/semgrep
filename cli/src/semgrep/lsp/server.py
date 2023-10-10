import logging
import subprocess
import sys
import threading
import time
from typing import Optional

from pylsp_jsonrpc.streams import JsonRpcStreamReader
from pylsp_jsonrpc.streams import JsonRpcStreamWriter

from semgrep.semgrep_core import SemgrepCore
from semgrep.types import JsonObject

log = logging.getLogger(__name__)


# This is essentially a "middleware" between the client and the core LS. Since
# core/osemgrep does't have login functionality, we need to do that stuff
# here.
class SemgrepCoreLSServer:
    def __init__(self) -> None:
        self.std_reader = JsonRpcStreamReader(sys.stdin.buffer)
        self.std_writer = JsonRpcStreamWriter(sys.stdout.buffer)
        self.core_process: Optional[subprocess.Popen] = None
        self.exit_code: int = 0
        self.polling: bool = True

    def start_ls(self) -> None:
        args = ["osemgrep", "lsp", "--debug"]
        executable = str(SemgrepCore.path())
        log.info(f"Starting Semgrep Language Server at path {executable}")
        self.core_process = subprocess.Popen(
            args,
            executable=executable,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )

        self.core_reader = JsonRpcStreamReader(self.core_process.stdout)
        self.core_writer = JsonRpcStreamWriter(self.core_process.stdin)

        # Listening for messages is blocking, and we could try and guess how
        # often core will respond and ready it synchronously, but that'd be obnoxious
        def core_handler() -> None:
            self.core_reader.listen(self.on_core_message)

        def core_poller() -> None:
            while self.polling:
                self.poll_ls()
                time.sleep(1)

        self.thread = threading.Thread(target=core_handler)
        self.thread.daemon = True
        self.thread.start()

        self.poll_thread = threading.Thread(target=core_poller)
        self.poll_thread.daemon = True
        self.poll_thread.start()

    def poll_ls(self) -> None:
        if not self.core_process:
            return
        status = self.core_process.poll()
        if status is not None and status != 0:
            self.exit_code = status
            self.polling = False
            self.stop()

    def on_std_message(self, msg: JsonObject) -> None:
        self.core_writer.write(msg)

    def on_core_message(self, msg: JsonObject) -> None:
        self.std_writer.write(msg)

    def start(self) -> None:
        """Start the json-rpc endpoint"""
        self.std_reader.listen(self.on_std_message)

    def stop(self) -> None:
        """Stop the language server"""
        if self.core_process and self.core_process.poll() is None:
            self.core_writer.write({"jsonrpc": "2.0", "method": "shutdown", "id": 1})
            self.core_process.wait()
        try:
            self.core_reader.close()
            self.core_writer.close()
            self.poll_thread.join()
            self.thread.join()
        except Exception:
            pass


def run_server() -> None:
    log.info("Starting Semgrep language server?")
    server = SemgrepCoreLSServer()
    try:
        server.start()
    finally:
        server.stop()
    log.info("Server stopped!")
    sys.exit(server.exit_code)
