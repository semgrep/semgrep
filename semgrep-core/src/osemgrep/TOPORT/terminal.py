class Terminal:
    """Fundamental settings on how to log output.

    TODO: Consider the naming and boundaries of what this vs. other classes do.
          We have 3 layers of output settings now: Terminal, OutputSettings, OutputHandler.
    """

    force_color_on: bool = False
    force_color_off: bool = False

    def configure(
        self,
        *,
        force_color: bool = False,
    ) -> None:

        stdout_level = logging.INFO
        if verbose:
            stdout_level = logging.VERBOSE  # type: ignore[attr-defined]
        elif quiet:
            stdout_level = logging.CRITICAL

        # Setup file logging
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
