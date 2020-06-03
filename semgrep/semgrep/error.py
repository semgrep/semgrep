class SemgrepError(Exception):
    """
    Parent class of all exceptions we anticipate in Semgrep commands

    All Semgrep Exceptions are caught and their error messages
    are displayed to the user.
    """

    def __init__(self, *args: object, code: int = 1) -> None:
        self.code = code

        super().__init__(*args)


class InvalidRuleSchemaError(SemgrepError):
    pass


class InvalidPatternNameError(SemgrepError):
    pass


class UnknownOperatorError(SemgrepError):
    pass
