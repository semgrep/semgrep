from enum import auto
from enum import Enum
from typing import List

import click
from attrs import Factory
from attrs import frozen

from semgrep.app.session import AppSession
from semgrep.env import Env
from semgrep.error_handler import ErrorHandler
from semgrep.metrics import Metrics
from semgrep.settings import Settings
from semgrep.terminal import Terminal


class DesignTreatment(Enum):
    LEGACY = auto()  # default
    SIMPLE = auto()  # simple output for product-focused users
    DETAILED = auto()  # detailed output for power users


@frozen
class SemgrepState:
    """
    An object click keeps around as custom global state for the current CLI invocation.

    This replaces the way we used to keep singletons around on the module level.
    """

    app_session: AppSession = Factory(AppSession)
    env: Env = Factory(Env)
    metrics: Metrics = Factory(Metrics)
    error_handler: ErrorHandler = Factory(ErrorHandler)
    settings: Settings = Factory(Settings)
    terminal: Terminal = Factory(Terminal)

    @staticmethod
    def get_cli_ux_flavor() -> DesignTreatment:
        """
        Returns the CLI UX flavor to use for the current CLI invocation.
        """
        new_cli_ux = get_state().env.with_new_cli_ux
        if not new_cli_ux:
            return DesignTreatment.LEGACY
        config = get_config()
        # NOTE: We only support simple and detailed UX treatments for `semgrep scan` invocations
        if SemgrepState.is_scan_invocation():
            # NOTE: Ignore the default 'auto' config and product flags such as 'supply-chain'
            has_config = bool(set(config) - {"auto", "supply-chain"})
            return DesignTreatment.DETAILED if has_config else DesignTreatment.SIMPLE
        return DesignTreatment.DETAILED

    @staticmethod
    def is_scan_invocation() -> bool:
        """
        Returns True iff the current CLI invocation is a scan invocation via `semgrep scan`.
        """
        ctx = get_context()
        command_name = ctx.command.name if hasattr(ctx, "command") else "unset"
        return command_name == "scan"


def get_context() -> click.Context:
    """
    Get the current CLI invocation's click context.
    """
    ctx = click.get_current_context(silent=True)
    if ctx is None:
        # create a dummy context that will never be torn down
        from semgrep.cli import cli  # avoiding circular import

        ctx = click.Context(command=cli).scope().__enter__()

    return ctx


def get_config() -> List[str]:
    """
    Get the config passed via command line arguments (click)
    that is in turn passed to semgrep-core and friends.
    """
    ctx = get_context()
    params = ctx.params if hasattr(ctx, "params") else {}
    return list(params.get("config") or ())


def get_state() -> SemgrepState:
    """
    Get the current CLI invocation's global state.
    """
    ctx = get_context()
    return ctx.ensure_object(SemgrepState)
