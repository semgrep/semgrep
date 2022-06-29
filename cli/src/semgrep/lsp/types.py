from enum import Enum
from typing import List

from semgrep.types import JsonObject


CodeActionContext = JsonObject
CodeActionsList = List[JsonObject]
Diagnostic = JsonObject
DiagnosticsList = List[Diagnostic]
Range = JsonObject
TextDocumentItem = JsonObject


class DiagnosticSeverity(Enum):
    Error = 1
    Warning = 2
    Information = 3
    Hint = 4
