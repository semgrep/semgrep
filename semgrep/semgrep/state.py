import click
from attrs import Factory
from attrs import frozen

from semgrep.app.session import AppSession


@frozen
class SemgrepState:
    """
    An object click keeps around as custom global state for the current CLI invocation.

    This replaces the way we used to keep singletons around on the module level.
    """

    app_session: AppSession = Factory(AppSession)


def get_state() -> SemgrepState:
    """
    Get the current CLI invocation's global state.
    """
    state = click.get_current_context().obj

    if not isinstance(state, SemgrepState):
        raise RuntimeError(
            "Tried to access CLI state, but it hasn't been initialized yet."
        )

    return state
