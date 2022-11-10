def handle_command_errors(func: Callable) -> Callable:
    """
    Adds the following functionality to our subcommands:
    - Enforces that exit code 1 is only for findings
    - Sets global logging level
    - Handles metric sending before exit
    """
    def wrapper(*args: Any, **kwargs: Any) -> NoReturn:
        # When running semgrep as a command line tool
        # silence root level logger otherwise logs higher
        # than warning are handled twice
        logger = getLogger("semgrep")
        logger.propagate = False
        try:
            func(*args, **kwargs)
        finally:
            metrics = get_state().metrics
            metrics.add_exit_code(exit_code)
            metrics.send()

            error_handler = get_state().error_handler
            exit_code = error_handler.send(exit_code)
    return wrapper
