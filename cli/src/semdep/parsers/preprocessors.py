import re


class CommentRemover:
    def __init__(self, regex: str) -> None:
        self.pattern = re.compile(regex, re.MULTILINE)

    def __call__(self, target: str) -> str:
        return self.pattern.sub("", target)
