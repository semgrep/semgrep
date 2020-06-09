OK_EXIT_CODE = 0
FINDINGS_EXIT_CODE = 1
FATAL_EXIT_CODE = 2
INVALID_CODE_EXIT_CODE = 3
INVALID_PATTERN_EXIT_CODE = 4
UNPARSEABLE_YAML_EXIT_CODE = 5
NEED_ARBITRARY_CODE_EXEC_EXIT_CODE = 6
MISSING_CONFIG_EXIT_CODE = 7


class SemgrepError(Exception):
    """
    Parent class of all exceptions we anticipate in Semgrep commands

    All Semgrep Exceptions are caught and their error messages
    are displayed to the user.
    """

    def __init__(self, *args: object, code: int = FATAL_EXIT_CODE) -> None:
        self.code = code

        super().__init__(*args)


class InvalidRuleSchemaError(SemgrepError):
    pass


class InvalidPatternNameError(SemgrepError):
    pass


class UnknownOperatorError(SemgrepError):
    pass
