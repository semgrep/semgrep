import logging
import os
import sys

from attr import define

from semgrep.env import Env


@define
class Terminal:
    """Fundamental settings on how to log output.

    TODO: Consider the naming and boundaries of what this vs. other classes do.
          We have 3 layers of output settings now: Terminal, OutputSettings, OutputHandler.
    """

    force_color_on: bool = False
    force_color_off: bool = False
    log_level: int = logging.INFO

    def __attrs_post_init__(self) -> None:
        # provision with default config so that tests can immediately capture logging output
        self.configure()

    def init_for_cli(self) -> None:
        """Call this when semgrep is invoked as a CLI as opposed to a library."""
        self.configure(quiet=False)

    def configure(
        self,
        *,
        verbose: bool = False,
        debug: bool = False,
        quiet: bool = True,
        force_color: bool = False,
    ) -> None:
        """Set the relevant logging levels"""
        # Assumes only one of verbose, debug, quiet is True
        logger = logging.getLogger("semgrep")
        logger.handlers = []  # Reset to no handlers

        stdout_level = logging.INFO
        if verbose:
            stdout_level = logging.VERBOSE  # type: ignore[attr-defined]
        elif debug:
            stdout_level = logging.DEBUG
        elif quiet:
            stdout_level = logging.CRITICAL

        # Setup stdout logging
        stdout_handler = logging.StreamHandler()
        stdout_formatter = logging.Formatter("%(message)s")
        stdout_handler.setFormatter(stdout_formatter)
        stdout_handler.setLevel(stdout_level)
        logger.addHandler(stdout_handler)

        # Setup file logging
        # Env and Terminal get initialized together, but Terminal depends on env
        env = Env()
        # env.user_log_file dir must exist
        env.user_log_file.parent.mkdir(parents=True, exist_ok=True)
        file_handler = logging.FileHandler(env.user_log_file, "w")
        file_formatter = logging.Formatter(
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        )
        file_handler.setLevel(logging.DEBUG)
        file_handler.setFormatter(file_formatter)
        logger.addHandler(file_handler)

        # Needs to be DEBUG otherwise will filter before sending to handlers
        logger.setLevel(logging.DEBUG)

        self.log_level = stdout_level

        if force_color or os.environ.get("SEMGREP_FORCE_COLOR") is not None:
            self.force_color_on = True

        if (
            os.environ.get("NO_COLOR") is not None  # https://no-color.org/
            or os.environ.get("SEMGREP_FORCE_NO_COLOR") is not None
        ):
            self.force_color_off = True

    @property
    def is_quiet(self) -> bool:
        """
        Returns true if logging level is quiet or quieter (higher)
        (i.e. only critical logs surfaced)
        """
        return self.log_level >= logging.CRITICAL

    @property
    def is_debug(self) -> bool:
        """
        Returns true if logging level is debug or noisier (lower)
        (i.e. want more logs)
        """
        return self.log_level <= logging.DEBUG

    @property
    def is_color(self) -> bool:
        if self.force_color_on:
            # for 'no color' there is a global env var (https://no-color.org/)
            # while the env var to 'force color' is semgrep-specific
            # so force-on overrides force-off
            return True

        return sys.stderr.isatty() and not self.force_color_off
