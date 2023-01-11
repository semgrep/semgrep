class CliError(Exception, metaclass=abc.ABCMeta):
    """A user facing CLI error"""

    def __init__(self, message: str):
        self.message = message

class FileStore(metaclass=abc.ABCMeta):
    def foo():
        return 1

class AnalyzerOutputJson(_AnalyzerOutputJsonBase, total=False):
     finding_extra_schema: Dict[str, Any]
     error_extra_schema: Dict[str, Any]
