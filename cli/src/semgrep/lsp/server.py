import logging
import re
import sys
import urllib.request
import uuid
from typing import BinaryIO
from typing import List
from typing import MutableMapping
from typing import Optional

from pylsp_jsonrpc.dispatchers import MethodDispatcher
from pylsp_jsonrpc.endpoint import Endpoint
from pylsp_jsonrpc.streams import JsonRpcStreamReader
from pylsp_jsonrpc.streams import JsonRpcStreamWriter

from semgrep import __VERSION__ as SEMGREP_VERSION
from semgrep.lsp.config import LSPConfig
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
    "inlayHintProvider": False,
    "textDocumentSync": {
        "save": {
            "includeText": True,
        },
        "openClose": True,
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

    def start(self) -> None:
        # Start the json-rpc endpoint
        self._jsonrpc_stream_reader.listen(self._endpoint.consume)

    def stop(self) -> None:
        log.info("Server stopping")
        # Stop the json-rpc endpoint
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
        log.debug(
            f"Semgrep Language Server initialized with:\n"
            f"... PID {processId}\n"
            f"... rooUri {rootUri}\n... rootPath {rootPath}\n"
            f"... options {initializationOptions}\n"
            f"... workspaceFolders {workspaceFolders}",
        )
        config = initializationOptions if initializationOptions is not None else {}
        if workspaceFolders is not None:
            self.config = LSPConfig(config, workspaceFolders)
        else:
            self.config = LSPConfig(config, [{"name": "root", "uri": rootUri}])
        # Get our capabilities
        return {
            "capabilities": SERVER_CAPABILITIES,
            "serverInfo": {
                "name": "semgrep",
                "version": SEMGREP_VERSION,
            },
        }

    def m_initialized(self, **_kwargs: JsonObject) -> None:
        log.info("Semgrep LSP initialized")
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
        self._ready = True

    def m_shutdown(self, **_kwargs: JsonObject) -> None:
        log.info("Server shutting down")
        self._ready = False

    # TODO: VSCode seems to close the streams right after the shutdown message, which
    # makes us exit anyway, instead of sending an exit message.
    def m_exit(self, **_kwargs: JsonObject) -> None:
        log.info("Server stopping")
        self.stop()

    def m_text_document__did_close(
        self, textDocument: Optional[TextDocumentItem] = None, **_kwargs: JsonObject
    ) -> None:
        log.debug("document__did_close")
        if self._ready and textDocument is not None:
            self.cleanup_diagnostics(textDocument)

    def m_text_document__did_change(
        self, textDocument: Optional[TextDocumentItem] = None, **_kwargs: JsonObject
    ) -> None:
        log.debug("document__did_open")
        if self._ready and textDocument is not None:
            self.process_text_document(textDocument)

    def m_text_document__did_open(
        self, textDocument: Optional[TextDocumentItem] = None, **_kwargs: JsonObject
    ) -> None:
        log.debug("document__did_open")
        if self._ready and textDocument is not None:
            self.process_text_document(textDocument)

    def m_text_document__did_save(
        self, textDocument: Optional[TextDocumentItem] = None, **_kwargs: JsonObject
    ) -> None:
        log.debug(f"document__did_save")
        if self._ready and textDocument is not None:
            self.process_text_document(textDocument)

    def m_text_document__code_action(
        self,
        textDocument: Optional[TextDocumentItem] = None,
        range: Optional[Range] = None,
        context: Optional[CodeActionContext] = None,
        **_kwargs: JsonObject,
    ) -> CodeActionsList:
        if self._ready and textDocument is not None and range is not None:
            return self.compute_code_actions(textDocument["uri"], range)
        else:
            return []

    def m_text_document__inlay_hint(
        self,
        textDocument: Optional[TextDocumentItem] = None,
        range: Optional[Range] = None,
        **_kwargs: JsonObject,
    ) -> List[JsonObject]:
        if self._ready and textDocument is not None and range is not None:
            return self.compute_inlay_hints(textDocument["uri"], range)
        else:
            return []

    #
    # LSP Workspace methods
    #

    def m_workspace__workspace_folders(
        self,
        workspace_folders: Optional[List[JsonObject]] = None,
        **_kwargs: JsonObject,
    ) -> None:
        if workspace_folders is not None:
            self.config._workspace_folders = workspace_folders

    def m_workspace__did_change_workspace_folders(self, event: JsonObject) -> None:
        self.config.update_workspace(event["added"], event["removed"])
        if self.config.watch_workspace:
            self.process_workspaces()

    def m_workspace__did_change_watched_files(self, changes: JsonObject) -> None:
        if self.config.watch_workspace:
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

    #
    #
    #

    def register_capability(
        self, method: str, registerOptions: Optional[JsonObject] = None
    ) -> None:
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
        self._endpoint.request(
            "client/unregisterCapability",
            {
                "unregistrations": [
                    {"id": self._registered_capabilities[method], "method": method}
                ]
            },
        )

    def publish_diagnostics(self, uri: str, diagnostics: DiagnosticsList) -> None:
        self._endpoint.notify(
            "textDocument/publishDiagnostics",
            {
                "uri": uri,
                "diagnostics": diagnostics,
            },
        )

    def refresh_inlay_hints(self) -> None:
        self._endpoint.request(
            "workspace/inlayHint/refresh",
        )

    def process_text_document(self, textDocument: TextDocumentItem) -> None:
        log.debug("textDocument: %s", textDocument)

        uri = urllib.parse.urlparse(textDocument["uri"])
        if uri.scheme != "file":
            return
        target_name = urllib.request.url2pathname(uri.path)
        log.info(f"Running Semgrep on {target_name}")

        diagnostics = rule_match_map_to_diagnostics(
            run_rules([target_name], self.config)
        )
        self._diagnostics[textDocument["uri"]] = diagnostics
        self.publish_diagnostics(textDocument["uri"], diagnostics)
        self.refresh_inlay_hints()

    def process_workspaces(self) -> None:
        log.info(f"Running Semgrep on workspaces {self.config.folder_paths}")
        for uri in self._diagnostics:
            self.publish_diagnostics(uri, [])
        self._diagnostics = {}
        diagnostics = rule_match_map_to_diagnostics(
            run_rules(self.config.folder_paths, self.config)
        )
        sorted_diagnostics: MutableMapping[str, List[JsonObject]] = {}
        for d in diagnostics:
            if d["data"]["uri"] not in sorted_diagnostics:
                sorted_diagnostics[d["data"]["uri"]] = []
            sorted_diagnostics[d["data"]["uri"]].append(d)
        self._diagnostics = sorted_diagnostics
        for uri in self._diagnostics:
            self.publish_diagnostics(uri, self._diagnostics[uri])
        self.refresh_inlay_hints()

    def cleanup_diagnostics(self, textDocument: TextDocumentItem) -> None:
        self._diagnostics[textDocument["uri"]] = []
        self.publish_diagnostics(textDocument["uri"], [])
        self.refresh_inlay_hints()

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

    @staticmethod
    def text_ranges_overlap(range1: Range, range2: Range) -> bool:
        return bool(
            range1["start"]["line"] < range2["end"]["line"]
            and range1["end"]["line"] > range2["start"]["line"]
        ) or bool(
            range1["start"]["line"] <= range2["end"]["line"]
            and range1["end"]["line"] >= range2["start"]["line"]
            and range1["start"]["character"] <= range2["end"]["character"]
            and range1["end"]["character"] >= range2["start"]["character"]
        )

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
                        hint: JsonObject = {
                            "position": {
                                "line": info.start.line - 1,
                                "character": info.start.col - 1,
                            },
                            "label": f"{metavar}:",
                            "tooltip": info.abstract_content,
                            "paddingRight": True,
                        }
                        if hint not in hints:
                            hints.append(hint)

        log.debug(f"Computed inlay hints: {hints}")
        return hints


def run_server() -> None:
    log.info("Starting Semgrep language server.")
    server = SemgrepLSPServer(sys.stdin.buffer, sys.stdout.buffer)
    server.start()
    log.info("Server stopped!")
