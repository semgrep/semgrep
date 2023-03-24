import re


class CommentRemover:
    def __init__(self, regex: str = r"(^|\s+)#.*($)") -> None:
        self.pattern = re.compile(regex, re.MULTILINE)
        if self.pattern.groups != 2:
            raise ValueError(
                "A comment remover regex pattern must have exactly two capture groups. "
                "Group 1 is text before comment to keep, group 2 is text after comment to keep. "
                "This helps us keep accurate line numbers for matches after preprocessing. "
                f"Your pattern has {self.pattern.groups} instead of 2 groups and looks like {repr(self.pattern)}"
            )

    def __call__(self, target: str) -> str:
        return self.pattern.sub(r"\1\2", target)
