from enum import Enum

from semgrep.types import JsonObject


CodeActionContext = JsonObject
CodeAction = JsonObject
Diagnostic = JsonObject
Range = JsonObject
TextDocumentItem = JsonObject


class DiagnosticSeverity(Enum):
    Error = 1
    Warning = 2
    Information = 3
    Hint = 4
