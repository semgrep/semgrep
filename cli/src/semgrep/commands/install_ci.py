import click
from semgrep.commands.wrapper import handle_command_errors

@click.command()
@handle_command_errors
def install_semgrep_ci() -> None:
    """
    Install Semgrep CI in Github Actions

    Must be logged in to use; see `semgrep login`

    Visit https://semgrep.dev/deep-semgrep-beta for more information
    """
    return "Hello, world!"
