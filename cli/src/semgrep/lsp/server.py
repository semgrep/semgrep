from builtins import Exception
import subprocess
import threading
import time
import json
import contextlib
from tempfile import _TemporaryFileWrapper
import tempfile
import logging

import sys
from pylsp_jsonrpc.streams import JsonRpcStreamReader
from pylsp_jsonrpc.streams import JsonRpcStreamWriter

import urllib
from typing import Optional
from typing import List
from typing import Any

from semgrep.state import get_state
from semgrep.app import auth
from semgrep.core_runner import CoreRunner
from semgrep.lsp.config import LSPConfig
from semgrep.types import JsonObject
from semgrep.commands.login import make_login_url

log = logging.getLogger(__name__)

# A lot of this could be put in core runner, but once ported that would be unnecessary
# and putting it there would require a lot of changes (so stuff doesn't print out).
#
class SemgrepCoreLSPServer():
    def __init__(self, rule_file:_TemporaryFileWrapper, target_file: _TemporaryFileWrapper) -> None:
        self.std_reader = JsonRpcStreamReader(sys.stdin.buffer)
        self.std_writer = JsonRpcStreamWriter(sys.stdout.buffer)
        self.config = LSPConfig({},[])
        self.rule_file = rule_file
        self.target_file = target_file

    def update_targets_file(self) -> None:
        self.config.update_target_manager()
        self.plan = CoreRunner.plan_core_run(self.config.rules, self.config.target_manager, set())
        target_file_contents = json.dumps(self.plan.to_json())
        # So semgrep-core could try to open the target file when it's empty if
        # we just truncated then wrote it (since the truncate may write without
        # a flush call). If we simply write then truncate, it'll be ok because
        # the core will only take the first occurence of the target json in the
        # file. Weird
        self.target_file.seek(0,0)
        self.target_file.write(target_file_contents)
        self.target_file.truncate(len(target_file_contents))
        self.target_file.flush()

    def update_rules_file(self) -> None:
        try:
            rules = self.config.rules
            rule_file_contents = json.dumps(
                {"rules": [rule._raw for rule in rules]}, indent=2, sort_keys=True
            )
            self.rule_file.seek(0,0)
            self.rule_file.write(rule_file_contents)
            self.rule_file.truncate(len(rule_file_contents))
            self.rule_file.flush()
        except Exception as e:
            self.notify_show_message(3, f"Error updating rules: {e}")

    def start_ls(self) -> None:
        self.plan = CoreRunner.plan_core_run(self.config.rules, self.config.target_manager, set())

        self.update_targets_file()
        self.update_rules_file()

        cmd = ["semgrep-core",
            "-j", str(self.config.jobs),
            "-rules", self.rule_file.name,
            "-targets", self.target_file.name,
            "-max_memory",str(self.config.max_memory),
            "-fast",
            "-ls",
            "-debug"]
        self.core_process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            )

        self.core_reader = JsonRpcStreamReader(self.core_process.stdout)
        self.core_writer = JsonRpcStreamWriter(self.core_process.stdin)
        def core_handler() -> None:
            self.core_reader.listen(self.on_core_message)
        self.thread = threading.Thread(target=core_handler)
        self.thread.daemon = True
        self.thread.start()

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
        elif rootUri is not None:
            self.config = LSPConfig(config, [{"name": "root", "uri": rootUri}])
        else:
            self.config = LSPConfig(config, [])

        get_state()
        saved_login_token = auth.get_token()
        if not saved_login_token:
            self.notify_show_message(
                3,
                "Login to enable additional proprietary Semgrep Registry rules and running custom policies from Semgrep App",
            )
        self.start_ls()

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



    def init_login(self) -> JsonObject:
        session_id, url = make_login_url()
        return {"url": url, "sessionId": str(session_id)}

    def m_semgrep__login(self) -> Optional[JsonObject]:
        """Called by client to login to Semgrep App. Returns None if already logged in"""
        if self.config.logged_in:
            self.notify_show_message(3, "Already logged in to Semgrep Code")
            return None
        else:
            return self.init_login()

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
                if login_token is not None and auth.is_valid_token(login_token):
                    auth.set_token(login_token)
                    state = get_state()
                    state.app_session.authenticate()
                    self.notify_show_message(3, f"Successfully logged in to Semgrep Code")
                    self.update_rules_file()
                    self.update_targets_file()
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
        log.info(f"token: {self.config.token}")
        method = msg.get("method","")
        params = msg.get("params", {})
        if method == "initialize":
            self.m_initialize(**params)

        if method == "textDocument/didOpen":
            uri = urllib.parse.urlparse(params["textDocument"]["uri"])
            if urllib.request.url2pathname(uri.path) not in self.config.target_manager.get_all_files():
                self.update_targets_file()

        if method == "semgrep/login":
            log.info(f"{self.config.configs}")
            body = self.m_semgrep__login()
            response = {"id": msg["id"], "result": body}
            self.on_core_message(response)
            return
        if method == "semgrep/loginFinish":
            self.m_semgrep__login_finish(**params)
        self.core_writer.write(msg)

    def on_core_message(self, msg: JsonObject) -> None:
        self.std_writer.write(msg)

    def start(self) -> None:
        """Start the json-rpc endpoint"""
        self.std_reader.listen(self.on_std_message)


def run_server() -> None:
    log.info("Starting Semgrep language server.")
    exit_stack = contextlib.ExitStack()
    rule_file = exit_stack.enter_context(
        tempfile.NamedTemporaryFile("w+", suffix=".json")
    )
    target_file = exit_stack.enter_context(
        tempfile.NamedTemporaryFile("w+")
    )
    with exit_stack:
        server = SemgrepCoreLSPServer(rule_file, target_file)
        server.start()
    log.info("Server stopped!")
