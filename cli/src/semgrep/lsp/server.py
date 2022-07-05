import logging
import re
import sys
import urllib.request
import uuid
from typing import Any
from typing import BinaryIO
from typing import List
from typing import MutableMapping
from typing import Optional
from typing import Sequence

from pylsp_jsonrpc.dispatchers import MethodDispatcher
from pylsp_jsonrpc.endpoint import Endpoint
from pylsp_jsonrpc.streams import JsonRpcStreamReader
from pylsp_jsonrpc.streams import JsonRpcStreamWriter

from semgrep import __VERSION__ as SEMGREP_VERSION
from semgrep.lsp.config import LSPConfig
from semgrep.lsp.convert import metavar_to_inlay
from semgrep.lsp.convert import rule_match_map_to_diagnostics
from semgrep.lsp.run_semgrep import run_rules
from semgrep.lsp.types import CodeActionContext
from semgrep.lsp.types import CodeActionsList
from semgrep.lsp.types import Diagnostic
from semgrep.lsp.types import DiagnosticsList
from semgrep.lsp.types import Range
from semgrep.lsp.types import TextDocumentItem
from semgrep.semgrep_interfaces.semgrep_output_v0 import MetavarValue
from semgrep.types import JsonObject


log = logging.getLogger(__name__)


SERVER_CAPABILITIES = {
    "codeActionProvider": True,
    "inlayHintProvider": True,
    "textDocumentSync": {
        "save": {
            "includeText": False,
        },
        "openClose": True,
        "change": 2,
    },
    "workspaceFolders": {
        "supported": True,
        "changeNotifications": True,
    },
}


class SemgrepLSPServer(MethodDispatcher):  # type: ignore
    def __init__(self, rx: BinaryIO, tx: BinaryIO) -> None:
        # When set to False the server will ignore any incoming request,
        # beside a request to initialize or to exit.
        self._ready = False
        self.config = LSPConfig({}, [])

        # Prepare the json-rpc endpoint
        self._jsonrpc_stream_reader = JsonRpcStreamReader(rx)
        self._jsonrpc_stream_writer = JsonRpcStreamWriter(tx)
        self._endpoint = Endpoint(
            self, self._jsonrpc_stream_writer.write, max_workers=10
        )

        self._diagnostics: MutableMapping[str, DiagnosticsList] = {}
        self._registered_capabilities: MutableMapping[str, str] = {}

    # Override the default implementation of __getitem__ So we can ensure no
    # methods besides initialize, initialized, shutdown, exit are called before
    # we are ready
    def __getitem__(self, key: str) -> Any:
        if (
            key not in ["initialize", "initialized", "shutdown", "exit"]
            and not self._ready
        ):
            # At some point we should override the Endpoint's request_callback
            # to actually be able to send error responses
            return {}
        return super().__getitem__(key)

    def start(self) -> None:
        """Start the json-rpc endpoint"""
        self._jsonrpc_stream_reader.listen(self._endpoint.consume)

    def stop(self) -> None:
        """Stop the json-rpc endpoint"""
        log.info("Server stopping")
        self._endpoint.shutdown()
        self._jsonrpc_stream_reader.close()
        self._jsonrpc_stream_writer.close()

    #
    # LSP protocol methods
    # The MethodDispatcher will call m_<message> methods where `message` is the
    # of the json-rpc message.
    #

    def m_initialize(
        self,
        processId: Optional[str] = None,
        rootUri: Optional[str] = None,
        rootPath: Optional[str] = None,
        initializationOptions: Optional[JsonObject] = None,
        workspaceFolders: Optional[List[JsonObject]] = None,
        **_kwargs: JsonObject,
    ) -> JsonObject:
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
        else:
            # We should probably check if rootUri is None
            self.config = LSPConfig(config, [{"name": "root", "uri": rootUri}])
        # Get our capabilities
        return {
            "capabilities": SERVER_CAPABILITIES,
            "serverInfo": {
                "name": "semgrep",
                "version": SEMGREP_VERSION,
            },
        }

    def m_initialized(self) -> None:
        """Called by client after ok response from initialize"""
        # At some point we should only watch config files specified for us
        # But then we wouldn't watch join rule config files so not sure
        if self.config.watch_configs:
            self.register_capability(
                "workspace/didChangeWatchedFiles",
                {
                    "watchers": [
                        {
                            "globPattern": "**/*.{yml,yaml}",
                        }
                    ]
                },
            )
            log.info("Registered config watchers")
        self._ready = True
        log.info("Semgrep LSP initialized")
        self.process_workspaces()

    def m_shutdown(self) -> None:
        log.info("Server shutting down")

    # TODO: VSCode seems to close the streams right after the shutdown message, which
    # makes us exit anyway, instead of sending an exit message.
    def m_exit(self) -> None:
        log.info("Server stopping")
        self.stop()

    def m_text_document__did_close(self, textDocument: TextDocumentItem) -> None:
        log.debug("document__did_close")
        self.cleanup_diagnostics(textDocument)

    # Anything in here should be relatively quick, we should not scan every
    # change for now since that could slow down the client
    def m_text_document__did_change(
        self, textDocument: TextDocumentItem, contentChanges: Sequence[JsonObject]
    ) -> None:
        """Called by client everytime a file is changed (but not necessarily saved)"""
        log.debug("document__did_open")
        # Remove all findings if the file is changed
        for c in contentChanges:
            range = c["range"]
            if self._diagnostics.get(textDocument["uri"]) is not None:
                diagnostics = []
                for d in self._diagnostics[textDocument["uri"]]:
                    if not self.text_ranges_overlap(range, d["range"]):
                        diagnostics.append(d)
                self._diagnostics[textDocument["uri"]] = diagnostics
                self.publish_diagnostics(textDocument["uri"], diagnostics)

        self.refresh_inlay_hints()

    def m_text_document__did_open(self, textDocument: TextDocumentItem) -> None:
        log.debug("document__did_open")
        # Assume that all opened files are in the workspace and covered by the
        # workspace scan
        if self.config.watch_workspace:
            log.info(f"First scan of {textDocument['uri']}, using preprocessed results")
            return
        self.process_text_document(textDocument)

    def m_text_document__did_save(self, textDocument: TextDocumentItem) -> None:
        log.debug(f"document__did_save")
        self.process_text_document(textDocument)

    def m_text_document__code_action(
        self, textDocument: TextDocumentItem, range: Range, context: CodeActionContext
    ) -> CodeActionsList:
        return self.compute_code_actions(textDocument["uri"], range)

    # This is called by the client, but if we want them to know we have new
    # inlays we can send a notification with self.refresh_inlay_hints() and
    # they'll make a request to here
    def m_text_document__inlay_hint(
        self,
        textDocument: TextDocumentItem,
        range: Range,
    ) -> List[JsonObject]:
        """Called by client to get inlay hints. Use self.refresh_inlay_hints() to prompt them to request new hints"""
        return self.compute_inlay_hints(textDocument["uri"], range)

    #
    # LSP Workspace methods
    #

    def m_workspace__workspace_folders(
        self,
        workspace_folders: Optional[List[JsonObject]] = None,
    ) -> None:
        if workspace_folders is not None:
            self.config._workspace_folders = workspace_folders

    def m_workspace__did_change_workspace_folders(self, event: JsonObject) -> None:
        self.config.update_workspace(event["added"], event["removed"])
        # We scan here every time since workspace folders RARELY change
        if self.config.watch_workspace:
            self.process_workspaces()

    # This is called by the client when whatever files we registered above in
    # m_initialized changes. Handy for configs!
    def m_workspace__did_change_watched_files(self, changes: JsonObject) -> None:
        """Called by client when watched config files change"""
        # We're only watching config files so we don't care about the changes
        if self.config.watch_workspace:
            self.process_workspaces()

    def m_workspace__did_change_configuration(self, settings: JsonObject) -> None:
        """Called by client when settings change"""
        self.config = LSPConfig(settings, self.config._workspace_folders)
        self.process_workspaces()

    #
    #
    #

    # Custom commands to add:
    # - semgrep/workspaceRules (list of active rules)
    # - semgrep/documentRules (list of active rules for a document)
    # - semgrep/refreshRules (refresh the cached CI + Registry rules)
    # - semgrep/login (login to CI)
    # - semgrep/dump_ast (dump the AST for a file)
    # - semgrep/search

    # See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#client_registerCapability since this can be trick to understand
    def register_capability(
        self, method: str, registerOptions: Optional[JsonObject] = None
    ) -> None:
        """General method to register a capability with the client. Some capabilities such as watched files are recommended not to be done statically, but to be done dynamically through this method."""
        # Each capability needs a unique id, so we'll just generate one and
        # keep a map of ids to methods for easy unregistering
        id = str(uuid.uuid4())
        self._registered_capabilities[method] = id
        self._endpoint.request(
            "client/registerCapability",
            {
                "registrations": [
                    {"id": id, "method": method, "registerOptions": registerOptions}
                ]
            },
        )

    def unregister_capability(self, method: str) -> None:
        """Unregister a capability. See register_capability for more info."""
        self._endpoint.request(
            "client/unregisterCapability",
            {
                "unregistrations": [
                    {"id": self._registered_capabilities[method], "method": method}
                ]
            },
        )

    # The client DOES NOT do any merging of diagnostics, and to clear them ALL
    # out you must send an empty list explicitly.
    def publish_diagnostics(self, uri: str, diagnostics: DiagnosticsList) -> None:
        """Publish diagnostics to the client"""
        self._endpoint.notify(
            "textDocument/publishDiagnostics",
            {
                "uri": uri,
                "diagnostics": diagnostics,
            },
        )

    def cleanup_diagnostics(self, textDocument: TextDocumentItem) -> None:
        """Clear all diagnostics for a given file"""
        self._diagnostics[textDocument["uri"]] = []
        self.publish_diagnostics(textDocument["uri"], [])
        self.refresh_inlay_hints()

    def refresh_inlay_hints(self) -> None:
        """Prompt the client to ask for new inlay hints"""
        self._endpoint.request(
            "workspace/inlayHint/refresh",
        )

    def notify_work_done_create(self, message: str, title: str, token: str) -> str:
        res = self._endpoint.request(
            "window/workDoneProgress/create",
            {
                "token": token,
            },
        )
        self._endpoint.notify(
            "$/progress",
            {
                "token": token,
                "value": {
                    "kind": "begin",
                    "title": title,
                    "message": message,
                    "cancellable": False,
                },
            },
        )

        self._endpoint.notify(
            "$/progress",
            {
                "token": token,
                "value": {
                    "kind": "report",
                    "message": message,
                    "cancellable": False,
                },
            },
        )
        return token

    def notify_work_done_end(self, token: str, message: str) -> None:
        self._endpoint.notify(
            "$/progress",
            {
                "token": token,
                "value": {
                    "kind": "end",
                    "message": message,
                },
            },
        )
        log.debug(f"Ended notification {token}")

    def process_text_document(self, textDocument: TextDocumentItem) -> None:
        """Scan textDocument and publish diagnostics. This is called every time a file is saved or opened"""
        log.debug("textDocument: %s", textDocument)

        uri = urllib.parse.urlparse(textDocument["uri"])
        if uri.scheme != "file":
            return
        target_name = urllib.request.url2pathname(uri.path)
        log.info(f"Running Semgrep on {target_name} with configs {self.config.configs}")
        self.lsp_scan([target_name])

    def process_workspaces(self) -> None:
        """Scan workspace folders and recalculate all diagnostics. This is called every time the workspace changes"""
        log.info(f"Running Semgrep on workspaces {self.config.folder_paths}")
        for uri in self._diagnostics:
            self.publish_diagnostics(uri, [])
        self._diagnostics = {}
        self.lsp_scan(self.config.folder_paths)

    def lsp_scan(self, targets: List[str]) -> None:
        """Run a scan on targets and update diagnostics"""
        token = str(uuid.uuid4())
        self.notify_work_done_create(
            f"Scanning files {len(targets)} location(s)",
            "Running Semgrep",
            token,
        )
        # Run a scan on the file and convert to LSP diagnostics
        diagnostics = rule_match_map_to_diagnostics(run_rules(targets, self.config))
        self.notify_work_done_end(token, "Scanning complete")
        sorted_diagnostics: MutableMapping[str, List[JsonObject]] = {}
        # Record them so we can calculate fixes and inlay hints
        for d in diagnostics:
            if d["data"]["uri"] not in sorted_diagnostics:
                sorted_diagnostics[d["data"]["uri"]] = []
            sorted_diagnostics[d["data"]["uri"]].append(d)
        for t in sorted_diagnostics:
            self._diagnostics[t] = sorted_diagnostics[t]
            self.publish_diagnostics(t, sorted_diagnostics[t])
        self.refresh_inlay_hints()

    # Create a fix item for a given diagnostic. These are just autofixes! We
    # probably want to move this to convert at some point and make it more
    # typed
    def create_fix_action(
        self, uri: str, diagnostic: Diagnostic, newText: str
    ) -> JsonObject:
        check_id = diagnostic["code"]
        fix_action = {
            "title": f"Apply fix suggested by Semgrep rule {check_id}",
            "kind": "quickfix",
            "edit": {
                "changes": {uri: [{"range": diagnostic["range"], "newText": newText}]}
            },
        }
        return fix_action

    # Helper method since a lot of request from the client aren't for a whole
    # document but only for where the client is currently working.
    @staticmethod
    def text_ranges_overlap(range1: Range, range2: Range) -> bool:
        # Check exclusive bounds, as sometimes the client sends just line
        # numbers no characters
        return bool(
            range1["start"]["line"] < range2["end"]["line"]
            and range1["end"]["line"] > range2["start"]["line"]
            # Check inclusive bounds
        ) or bool(
            range1["start"]["line"] <= range2["end"]["line"]
            and range1["end"]["line"] >= range2["start"]["line"]
            and range1["start"]["character"] <= range2["end"]["character"]
            and range1["end"]["character"] >= range2["start"]["character"]
        )

    # Compute the fixes for a given document. Basically pull out all autofixes
    # from diagnostics.
    def compute_code_actions(self, uri: str, range: Range) -> CodeActionsList:
        log.debug(f"Compute code actions for uri {uri} and range {range}")
        diagnostics = self._diagnostics.get(uri)
        if not diagnostics:
            return []

        actions = []
        for d in diagnostics:
            if self.text_ranges_overlap(range, d["range"]):
                if "fix" in d["data"]:
                    actions.append(self.create_fix_action(uri, d, d["data"]["fix"]))
                if "fix_regex" in d["data"]:
                    fix_regex = d["data"]["fix_regex"]
                    source = d["data"]["matchSource"]
                    fix = re.sub(
                        fix_regex["regex"],
                        fix_regex["replacement"],
                        source,
                        count=fix_regex.get("count", 0),
                    )
                    actions.append(self.create_fix_action(uri, d, fix))

        log.debug(f"Computed code actions: {actions}")
        return actions

    def compute_inlay_hints(self, uri: str, range: Range) -> List[JsonObject]:
        """Compute inlay hints for a given document. This labels the abstract content associated with a metavar"""
        log.debug(f"Compute inlay hints for uri {uri} and range {range}")
        diagnostics = self._diagnostics.get(uri)
        if not diagnostics:
            return []

        hints = []
        for d in diagnostics:
            if self.text_ranges_overlap(d["range"], range):
                if "metavars" in d["data"]:
                    for metavar in d["data"]["metavars"]:
                        info = MetavarValue.from_json(d["data"]["metavars"][metavar])
                        hint = metavar_to_inlay(metavar, info)
                        if hint not in hints:
                            hints.append(hint)

        log.debug(f"Computed inlay hints: {hints}")
        return hints


def run_server() -> None:
    log.info("Starting Semgrep language server.")
    server = SemgrepLSPServer(sys.stdin.buffer, sys.stdout.buffer)
    server.start()
    log.info("Server stopped!")
