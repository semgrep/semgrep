import contextlib
import json
import logging
import subprocess
import sys
import tempfile
import threading
import time
import traceback
from tempfile import _TemporaryFileWrapper
from typing import Any
from typing import List
from typing import Optional

from pylsp_jsonrpc.streams import JsonRpcStreamReader
from pylsp_jsonrpc.streams import JsonRpcStreamWriter

from semgrep.app import auth
from semgrep.commands.login import make_login_url
from semgrep.core_runner import CoreRunner
from semgrep.error import SemgrepError
from semgrep.lsp.config import LSPConfig
from semgrep.state import get_state
from semgrep.types import JsonObject

log = logging.getLogger(__name__)

# This is essentially a "middleware" between the client and the core LS. Since
# core/osemgrep does't have the target manager yet, we need to do that stuff
# here.
class SemgrepCoreLSServer:
    def __init__(
        self, rule_file: _TemporaryFileWrapper, target_file: _TemporaryFileWrapper
    ) -> None:
        self.std_reader = JsonRpcStreamReader(sys.stdin.buffer)
        self.std_writer = JsonRpcStreamWriter(sys.stdout.buffer)
        self.config = LSPConfig({}, [])
        self.rule_file = rule_file
        self.target_file = target_file
        self.core_process: Optional[subprocess.Popen] = None
        self.exit_code: int = 0

    def update_targets_file(self) -> None:
        self.config.refresh_target_manager()
        self.plan = CoreRunner.plan_core_run(
            self.config.rules, self.config.target_manager, set()
        )
        target_file_contents = json.dumps(self.plan.to_json())
        # So semgrep-core could try to open the target file when it's empty if
        # we just truncated then wrote it (since the truncate may write without
        # a flush call). If we simply write then truncate, it'll be ok because
        # the core will only take the first occurence of the target json in the
        # file. Weird
        self.target_file.seek(0, 0)
        self.target_file.write(target_file_contents)
        self.target_file.truncate(len(target_file_contents))
        self.target_file.flush()

    def update_rules_file(self) -> None:
        self.config.refresh_rules()
        rules = self.config.rules
        rule_file_contents = json.dumps(
            {"rules": [rule._raw for rule in rules]}, indent=2, sort_keys=True
        )
        # Same as before re: sharing files with semgrep-core
        self.rule_file.seek(0, 0)
        self.rule_file.write(rule_file_contents)
        self.rule_file.truncate(len(rule_file_contents))
        self.rule_file.flush()

    def start_ls(self) -> None:
        cmd = [
            str(self.config.engine_type.get_binary_path()),
            "-j",
            str(self.config.jobs),
            "-rules",
            self.rule_file.name,
            "-targets",
            self.target_file.name,
            "-max_memory",
            str(self.config.max_memory),
            "-timeout",
            str(self.config.timeout),
            "-timeout_threshold",
            str(self.config.timeout_threshold),
            "-fast",
            "-ls",
        ]
        if self.config.debug:
            cmd.append("-debug")

        self.core_process = subprocess.Popen(
            cmd,
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
            while True:
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
                "Semgrep core process died, exiting...",
            )
            self.exit_code = status
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
        if workspaceFolders is not None:
            self.config = LSPConfig(config, workspaceFolders)
            if len(workspaceFolders) > 1:
                self.notify_show_message(
                    2,
                    "Running Semgrep Language Server with multiple workspace folders may degrade performance",
                )
        elif rootUri is not None:
            self.config = LSPConfig(config, [{"name": "root", "uri": rootUri}])
        else:
            self.config = LSPConfig(config, [])

        self.config.send_metrics()

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
                    self.update_rules_file()
                    self.update_targets_file()
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
        # We love a middleware
        id = msg.get("id", "")
        method = msg.get("method", "")
        params = msg.get("params", {})
        if method == "initialize":
            self.m_initialize(**params)
            self.start_ls()

        if method == "initialized":
            self.update_rules_file()
            self.update_targets_file()

        update_targets_methods = [
            "workspace/didCreateFiles",
            "workspace/didDeleteFiles",
            "workspace/didRenameFiles",
            "workspace/didChangeWorkspaceFolders",
        ]
        if method in update_targets_methods:
            self.update_targets_file()

        if method == "semgrep/login":
            self.m_semgrep__login(id)
            return

        if method == "semgrep/loginFinish":
            self.m_semgrep__login_finish(**params)

        if method == "semgrep/logout":
            auth.delete_token()
            self.notify_show_message(3, "Logged out of Semgrep Code")
            self.update_rules_file()
            self.update_targets_file()

        if method == "semgrep/refreshRules":
            self.update_rules_file()
            self.update_targets_file()

        if method == "semgrep/loginStatus":
            response = {"id": id, "result": {"loggedIn": self.config.logged_in}}
            self.on_core_message(response)
            return
        if method == "shutdown" or method == "exit":
            self.stop()
            return

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
        except Exception:
            pass

        self.config.send_metrics(self.exit_code, error)


def run_server() -> None:
    log.info("Starting Semgrep language server.")
    exit_stack = contextlib.ExitStack()
    rule_file = exit_stack.enter_context(
        tempfile.NamedTemporaryFile("w+", suffix=".json")
    )
    target_file = exit_stack.enter_context(tempfile.NamedTemporaryFile("w+"))
    server = SemgrepCoreLSServer(rule_file, target_file)
    exit_stack.callback(server.stop)
    with exit_stack:
        server.start()
    log.info("Server stopped!")
    sys.exit(server.exit_code)
