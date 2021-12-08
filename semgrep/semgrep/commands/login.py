import click

from semgrep.settings import SETTINGS

SEMGREP_LOGIN_TOKEN_ENVVAR_NAME = "SEMGREP_LOGIN_TOKEN"
SEMGREP_API_TOKEN_SETTINGS_KEY = "api_token"


@click.command()
def login() -> None:
    """
    Login flow or reads env var
    """
    login_token = SETTINGS.get_setting(SEMGREP_API_TOKEN_SETTINGS_KEY, default=None)
    if login_token:
        click.echo(
            f"API token already exists in {SETTINGS.get_path_to_settings()}. To login with a different token logout with `semgrep logout`"
        )
        return

    # TODO read env var
    # login_token = os.environ.get(SEMGREP_LOGIN_TOKEN_ENVVAR_NAME)

    # TODO validaton and retry?
    login_token = click.prompt("Enter semgrep.dev API token", hide_input=True)

    SETTINGS.add_setting(SEMGREP_API_TOKEN_SETTINGS_KEY, login_token)
    click.echo("Logged in")


@click.command()
def logout() -> None:
    """
    Remove all authentication tokens from user configuration
    """
    SETTINGS.delete_setting(SEMGREP_API_TOKEN_SETTINGS_KEY)
    click.echo("logged out")
