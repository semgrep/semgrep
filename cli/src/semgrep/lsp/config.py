from typing import Any
from typing import Dict
from typing import Mapping
from typing import Optional
from typing import Union

from semgrep.app import auth
from semgrep.engine import EngineType
from semgrep.error import SemgrepError
from semgrep.metrics import MetricsState
from semgrep.project import get_project_url
from semgrep.state import get_state
from semgrep.types import JsonObject


class LSPConfig:
    def __init__(self, lsp_config: JsonObject) -> None:
        lsp_config = dict(lsp_config)
        self._settings: Mapping[str, Mapping[str, Any]] = lsp_config

    # =====================
    # Semgrep LSP settings
    # =====================
    @property
    def extension_metrics(self) -> Dict[str, Any]:
        extension_metrics = self._settings.get("extensionMetrics", {})
        if isinstance(extension_metrics, dict):
            return extension_metrics
        elif isinstance(extension_metrics, str):
            # for backwards compatibility, older versions of the extension may send metrics: "on" or "off"
            return {
                "enabled": extension_metrics == "on",
            }
        else:
            return {}

    @property
    def metrics_state(self) -> MetricsState:
        choice = self.extension_metrics.get("enabled", True)
        if choice:
            return MetricsState.ON
        else:
            return MetricsState.OFF

    @property
    def project_url(self) -> Union[str, None]:
        return get_project_url()

    @property
    def token(self) -> Optional[str]:
        return auth.get_token()

    @property
    def logged_in(self) -> bool:
        return (
            self.token is not None
            and auth.get_deployment_from_token(self.token) is not None
        )

    @property
    def debug(self) -> bool:
        trace = self._settings.get("trace")
        debug = trace.get("server") if trace else None
        return debug is not None and debug == "verbose"

    @property
    def engine_type(self) -> EngineType:
        # Pro engine is too slow right now for editor integration
        return EngineType.OSS

    def send_metrics(
        self, exit_code: Optional[int] = None, error: Optional[SemgrepError] = None
    ) -> None:
        state = get_state()
        metrics = state.metrics
        metrics.configure(self.metrics_state, None)
        metrics.add_engine_type(self.engine_type)
        metrics.add_project_url(self.project_url)
        metrics.add_token(self.token)
        if exit_code is not None:
            metrics.add_exit_code(exit_code)
        if error is not None:
            metrics.add_errors([error])
        extension_metrics = self.extension_metrics
        machine_id = extension_metrics.get("machineId")
        new_install = extension_metrics.get("isNewAppInstall")
        session_id = extension_metrics.get("sessionId")
        version = extension_metrics.get("extensionVersion")
        type = extension_metrics.get("extensionType")
        metrics.add_extension(machine_id, new_install, session_id, version, type)
        metrics.send()
