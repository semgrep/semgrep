#
# Copyright (c) 2023-2024 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
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
