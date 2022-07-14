import logging
import sys
from typing import Optional

import click

from semgrep.commands.wrapper import handle_command_errors
from semgrep.lsp.server import run_server


def init_log(name: str, level: int, logfile: Optional[str] = None) -> None:
    """
    Set logging to avoid stdout as the LSP server needs exclusive use of it.
    """
    log = logging.getLogger(name)
    log.setLevel(level)
    handler: logging.Handler = logging.StreamHandler(stream=sys.stderr)
    if logfile:
        handler = logging.FileHandler(logfile)
    handler.setFormatter(
        logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    )
    log.addHandler(handler)


@click.command()
@click.option("--verbose", is_flag=True, help="Enable verbose logging")
@click.option("--debug", is_flag=True, help="Enable debug logging")
@click.option(
    "--logfile",
    "-l",
    type=click.Path(writable=True, exists=False, file_okay=True, dir_okay=False),
    required=False,
    help="Write logs to this file",
)
@handle_command_errors
def lsp(verbose: Optional[bool], debug: Optional[bool], logfile: Optional[str]) -> None:
    """
    [EXPERIMENTAL] Start the Semgrep LSP server.
    """

    # Setup logging
    level = logging.WARNING
    if verbose:
        level = logging.INFO
    elif debug:
        level = logging.DEBUG
    init_log("semgrep.lsp", level, logfile)

    print(
        "Starting Semgrep LSP server. This mode is experimental and prone to bugs or changes without notice."
    )
    run_server()
