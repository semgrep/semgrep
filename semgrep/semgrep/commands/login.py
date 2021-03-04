import os
import webbrowser

import click

from semgrep.authentication import Authentication

SEMGREP_LOGIN_TOKEN_ENVVAR_NAME = "SEMGREP_LOGIN_TOKEN"


@click.command()
def login():
    """
    Login flow or reads env var
    """
    configuration = Authentication()
    configuration.load()
    if configuration.token:
        click.echo(
            f"Auth token already exists in {Authentication.FILE_PATH}. To login with a different token logout with `semgrep logout`"
        )
        return

    login_token = os.environ.get(SEMGREP_LOGIN_TOKEN_ENVVAR_NAME)
    if not login_token:
        url = "https://semgrep.dev/manage/settings"
        if click.confirm(
            "Opening web browser to get login token. Do you want to continue?",
            default=True,
        ):
            click.echo(f"trying to open {url} in your browser...")
            try:
                webbrowser.open(url, new=0, autoraise=True)
            except Exception:
                click.echo(
                    f"Unable to open a web browser. Please visit {url} and paste the token in here"
                )
        else:
            # TODO go to user token page
            click.echo(f"Visit {url} and enter the token below")

        # TODO validaton and retry?
        login_token = click.prompt("Please enter the API token", hide_input=True)

    configuration.token = login_token
    configuration.save()


@click.command()
def logout():
    """
    Remove all authentication tokens from user configuration
    """
    # TODO error handling?
    Authentication.delete()
