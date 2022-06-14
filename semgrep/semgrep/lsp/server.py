import json
import logging
import re
import sys
import urllib.request
from os import path
from typing import BinaryIO
from typing import MutableMapping
from typing import Optional
from typing import Sequence

from pylsp_jsonrpc.dispatchers import MethodDispatcher
from pylsp_jsonrpc.endpoint import Endpoint
from pylsp_jsonrpc.streams import JsonRpcStreamReader
from pylsp_jsonrpc.streams import JsonRpcStreamWriter

from semgrep import __VERSION__ as SEMGREP_VERSION
from semgrep import config_resolver
from semgrep.lsp.convert import findings_to_diagnostic
from semgrep.lsp.run_semgrep import run_rule
from semgrep.lsp.run_semgrep import RunContext
from semgrep.lsp.types import CodeActionContext
from semgrep.lsp.types import CodeActionsList
from semgrep.lsp.types import Diagnostic
from semgrep.lsp.types import DiagnosticsList
from semgrep.lsp.types import Range
from semgrep.lsp.types import TextDocumentItem
from semgrep.types import JsonObject


log = logging.getLogger(__name__)


SERVER_CAPABILITIES = {
    "codeActionProvider": True,
    "textDocumentSync": {
        "save": {
            "includeText": True,
        },
        "openClose": True,
    },
}


class SemgrepLSPServer(MethodDispatcher):  # type: ignore
    def __init__(self, rx: BinaryIO, tx: BinaryIO, config_str: str) -> None:
        # When set to False the server will ignore any incoming request,
        # beside a request to initialize or to exit.
        self._ready = False

        # Prepare the json-rpc endpoint
        self._jsonrpc_stream_reader = JsonRpcStreamReader(rx)
        self._jsonrpc_stream_writer = JsonRpcStreamWriter(tx)
        self._endpoint = Endpoint(
            self, self._jsonrpc_stream_writer.write, max_workers=10
        )

        configs = list(config_resolver.ConfigPath(config_str).resolve_config().values())
        self._rules = json.dumps(configs[0].unroll_dict())

        self._diagnostics: MutableMapping[str, DiagnosticsList] = {}

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
        workspaceFolders: Optional[Sequence[str]] = None,
        **_kwargs: JsonObject,
    ) -> JsonObject:
        log.debug(
            f"Semgrep Language Server initialized with:\n"
            f"... PID {processId}\n"
            f"... rooUri {rootUri}\n... rootPath {rootPath}\n"
            f"... options {initializationOptions}\n"
            f"... workspaceFolders {workspaceFolders}",
        )

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
            self.cleanupDiagnostics(textDocument)

    def m_text_document__did_open(
        self, textDocument: Optional[TextDocumentItem] = None, **_kwargs: JsonObject
    ) -> None:
        log.debug("document__did_open")
        if self._ready and textDocument is not None:
            self.processTextDocument(textDocument)

    def m_text_document__did_save(
        self, textDocument: Optional[TextDocumentItem] = None, **_kwargs: JsonObject
    ) -> None:
        log.debug(f"document__did_save")
        if self._ready and textDocument is not None:
            self.processTextDocument(textDocument)

    def m_text_document__code_action(
        self,
        textDocument: Optional[TextDocumentItem] = None,
        range: Optional[Range] = None,
        context: Optional[CodeActionContext] = None,
        **_kwargs: JsonObject,
    ) -> CodeActionsList:
        if self._ready and textDocument is not None and range is not None:
            return self.computeCodeActions(textDocument["uri"], range)
        else:
            return []

    #
    #
    #

    def publishDiagnostics(self, uri: str, diagnostics: DiagnosticsList) -> None:
        self._endpoint.notify(
            "textDocument/publishDiagnostics",
            {
                "uri": uri,
                "diagnostics": diagnostics,
            },
        )

    def processTextDocument(self, textDocument: TextDocumentItem) -> None:
        log.debug("textDocument: %s", textDocument)

        uri = urllib.parse.urlparse(textDocument["uri"])
        if uri.scheme != "file":
            return

        target_name = urllib.request.url2pathname(uri.path)
        with open(target_name) as f:
            target_content = f.read()

        context = RunContext(
            "",
            [{"name": path.basename(target_name), "content": target_content}],
            {"strict": "true", "no_rewrite_rule_ids": "true"},
            {"output_time": "false"},
        )

        log.info(f"Running Semgrep on {target_name}")

        output = run_rule(self._rules, context)
        log.debug(f"Semgrep results:\n\n{output}")

        diagnostics = []
        results = output["results"]
        for r in results:
            diagnostics.append(findings_to_diagnostic(r, target_content))

        self._diagnostics[textDocument["uri"]] = diagnostics
        self.publishDiagnostics(textDocument["uri"], diagnostics)

    def cleanupDiagnostics(self, textDocument: TextDocumentItem) -> None:
        self._diagnostics[textDocument["uri"]] = []
        self.publishDiagnostics(textDocument["uri"], [])

    def createFixAction(
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
            range1["start"]["line"] <= range2["end"]["line"]
            and range1["end"]["line"] >= range2["start"]["line"]
            and range1["start"]["character"] <= range2["end"]["character"]
            and range1["end"]["character"] >= range2["start"]["character"]
        )

    def computeCodeActions(self, uri: str, range: Range) -> CodeActionsList:
        log.debug(f"Compute code actions for uri {uri} and range {range}")
        diagnostics = self._diagnostics.get(uri)
        if not diagnostics:
            return []

        actions = []
        for d in diagnostics:
            if self.text_ranges_overlap(range, d["range"]):
                if "fix" in d["data"]:
                    actions.append(self.createFixAction(uri, d, d["data"]["fix"]))
                if "fix_regex" in d["data"]:
                    fix_regex = d["data"]["fix_regex"]
                    source = d["data"]["matchSource"]
                    fix = re.sub(
                        fix_regex["regex"],
                        fix_regex["replacement"],
                        source,
                        count=fix_regex.get("count", 0),
                    )
                    actions.append(self.createFixAction(uri, d, fix))

        log.debug(f"Computed code actions: {actions}")
        return actions


def run_server(config: str) -> None:
    log.info("Starting Semgrep language server.")
    server = SemgrepLSPServer(sys.stdin.buffer, sys.stdout.buffer, config)
    server.start()
    log.info("Server stopped!")
