import click
from attrs import Factory
from attrs import frozen

from semgrep.app.session import AppSession
from semgrep.command import Command
from semgrep.env import Env
from semgrep.error_handler import ErrorHandler
from semgrep.metrics import Metrics
from semgrep.settings import Settings
from semgrep.terminal import Terminal


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
    command: Command = Factory(Command)


def get_state() -> SemgrepState:
    """
    Get the current CLI invocation's global state.
    """
    ctx = click.get_current_context(silent=True)
    if ctx is None:
        # create a dummy context that will never be torn down
        from semgrep.cli import cli  # avoiding circular import

        ctx = click.Context(command=cli).scope().__enter__()

    return ctx.ensure_object(SemgrepState)
