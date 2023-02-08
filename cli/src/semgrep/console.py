"""
This module provides an object for printing stuff nicely with rich's formatting features.

The rich docs recommend this usage pattern here:
https://rich.readthedocs.io/en/stable/console.html#console-api

See also the semgrep.terminal module which is an earlier attempt to standardize some output configuration,
but is more low level and doesn't really offload logic to other libraries.
"""
from typing import Literal
from attrs import frozen
from rich.console import Console
from rich.padding import Padding
from rich.panel import Panel
from rich.text import Text

console = Console(stderr=True)
console.width = min(console.width, 120)


@frozen
class Title:
    text: str
    order: Literal[1, 2, 3] = 1

    def __rich__(self):
        if self.order == 1:
            return Padding(Panel(self.text, expand=False), (2, 0, 1, 0))
        elif self.order == 2:
            return Padding(Text(self.text.upper(), style="underline"), (0, 2))
        elif self.order == 3:
            return Text("⮕ " + self.text.upper())


if __name__ == "__main__":
    """Print samples of the above components."""
    console.print("Semgrep output formatting samples:")
    console.print(Title("Level 1"))
    console.print(Title("Level 2", order=2))
    console.print(Title("Level 3", order=3))
