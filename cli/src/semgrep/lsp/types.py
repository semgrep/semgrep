from enum import Enum
from typing import Sequence

from semgrep.types import JsonObject


CodeActionContext = JsonObject
CodeActionsList = Sequence[JsonObject]
Diagnostic = JsonObject
DiagnosticsList = Sequence[Diagnostic]
Range = JsonObject
TextDocumentItem = JsonObject


class DiagnosticSeverity(Enum):
    Error = 1
    Warning = 2
    Information = 3
    Hint = 4
