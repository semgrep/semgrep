import logging
import subprocess
import sys
import threading
import time
import traceback
from typing import Any
from typing import List
from typing import Optional

from pylsp_jsonrpc.streams import JsonRpcStreamReader
from pylsp_jsonrpc.streams import JsonRpcStreamWriter

from semgrep.app import auth
from semgrep.commands.login import make_login_url
from semgrep.error import SemgrepError
from semgrep.lsp.config import LSPConfig
from semgrep.state import get_state
from semgrep.types import JsonObject

log = logging.getLogger(__name__)

# This is essentially a "middleware" between the client and the core LS. Since
# core/osemgrep does't have login functionality, we need to do that stuff
# here.
class SemgrepCoreLSServer:
    def __init__(self) -> None:
        self.std_reader = JsonRpcStreamReader(sys.stdin.buffer)
        self.std_writer = JsonRpcStreamWriter(sys.stdout.buffer)
        self.config = LSPConfig({})
        self.core_process: Optional[subprocess.Popen] = None
        self.exit_code: int = 0
        self.polling: bool = True

    def start_ls(self) -> None:
        args = ["osemgrep", "lsp", "--experimental"]
        executable = str(self.config.engine_type.get_binary_path())
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
            self.notify_show_message(
                1,
                f"OSemgrep process died with status {status}, exiting...",
            )
            self.exit_code = status
            self.polling = False
            self.stop()

    def notify(self, method: str, params: JsonObject) -> None:
        """Send a notification to the client"""
        jsonrpc_msg = {"jsonrpc": "2.0", "method": method, "params": params}
        self.std_writer.write(jsonrpc_msg)

    def notify_show_message(self, type: int, message: str) -> None:
        """Show a message to the user"""
        self.notify(
            "window/showMessage",
            {
                "type": type,
                "message": message,
            },
        )

    def m_initialize(
        self,
        processId: Optional[str] = None,
        rootUri: Optional[str] = None,
        rootPath: Optional[str] = None,
        initializationOptions: Optional[JsonObject] = None,
        workspaceFolders: Optional[List[JsonObject]] = None,
        **kwargs: Any,
    ) -> None:
        """Called by the client before ANYTHING else."""
        log.info(
            f"Semgrep Language Server initialized with:\n"
            f"... PID {processId}\n"
            f"... rooUri {rootUri}\n... rootPath {rootPath}\n"
            f"... options {initializationOptions}\n"
            f"... workspaceFolders {workspaceFolders}",
        )
        # Clients don't need to send initializationOptions :(
        config = initializationOptions if initializationOptions is not None else {}
        self.config = LSPConfig(config)
        self.config.send_metrics()
        self.start_ls()

    def m_semgrep__login(self, id: str) -> None:
        """Called by client to login to Semgrep App. Returns None if already logged in"""
        if self.config.logged_in:
            self.notify_show_message(3, "Already logged in to Semgrep Code")
        else:
            session_id, url = make_login_url()
            body = {"url": url, "sessionId": str(session_id)}
            response = {"id": id, "result": body}
            self.on_core_message(response)

    def m_semgrep__login_finish(self, url: str, sessionId: str) -> None:
        """Called by client to finish login to Semgrep App and save token"""
        WAIT_BETWEEN_RETRY_IN_SEC = 6
        MAX_RETRIES = 30
        self.notify_show_message(3, f"Waiting for login to Semgrep Code at {url}...")

        state = get_state()
        for _ in range(MAX_RETRIES):
            r = state.app_session.post(
                f"{state.env.semgrep_url}/api/agent/tokens/requests",
                json={"token_request_key": sessionId},
            )
            if r.status_code == 200:
                as_json = r.json()
                login_token = as_json.get("token")
                state = get_state()
                if login_token is not None and auth.get_deployment_from_token(
                    login_token
                ):
                    auth.set_token(login_token)
                    state = get_state()
                    state.app_session.authenticate()
                    self.notify_show_message(
                        3, f"Successfully logged in to Semgrep Code"
                    )
                else:
                    self.notify_show_message(1, f"Failed to log in to Semgrep Code")
                return
            elif r.status_code != 404:
                self.notify_show_message(
                    1,
                    f"Unexpected failure from {state.env.semgrep_url}: status code {r.status_code}; please contact support@r2c.dev if this persists",
                )

            time.sleep(WAIT_BETWEEN_RETRY_IN_SEC)

    def on_std_message(self, msg: JsonObject) -> None:
        id = msg.get("id", "")
        method = msg.get("method", "")
        params = msg.get("params", {})
        if method == "initialize":
            self.m_initialize(**params)

        if method == "semgrep/login":
            self.m_semgrep__login(id)
            return

        if method == "semgrep/loginFinish":
            self.m_semgrep__login_finish(**params)
        if method == "semgrep/loginStatus":
            response = {"id": id, "result": {"loggedIn": self.config.logged_in}}
            self.on_core_message(response)
            return
        if method == "semgrep/logout":
            auth.delete_token()
            self.notify_show_message(3, "Logged out of Semgrep Code")

        self.core_writer.write(msg)

    def on_core_message(self, msg: JsonObject) -> None:
        self.std_writer.write(msg)

    def start(self) -> None:
        """Start the json-rpc endpoint"""
        self.std_reader.listen(self.on_std_message)

    def stop(self) -> None:
        """Stop the language server"""
        exception = sys.exc_info()[1]
        error = None
        if exception is not None:
            self.notify_show_message(
                1, f"Error running Semgrep Language Server:\t{traceback.format_exc()}"
            )
            traceback.print_exc(file=sys.stderr)
            self.exit_code = 1
            error = SemgrepError(exception)
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

        self.config.send_metrics(self.exit_code, error)


def run_server() -> None:
    log.info("Starting Semgrep language server.")
    server = SemgrepCoreLSServer()
    try:
        server.start()
    finally:
        server.stop()
    log.info("Server stopped!")
    sys.exit(server.exit_code)
