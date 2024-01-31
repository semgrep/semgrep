"""
This module provides an object for printing stuff nicely with rich's
formatting features.

The rich docs recommend this usage pattern here:
https://rich.readthedocs.io/en/stable/console.html#console-api

See also the semgrep.terminal module which is an earlier attempt to
standardize some output configuration, but is more low level and
doesn't really offload logic to other libraries.
"""
from shutil import get_terminal_size
from typing import Any
from typing import Optional

from attrs import frozen
from rich import box
from rich.console import Console
from rich.console import Group
from rich.console import RenderableType
from rich.padding import Padding
from rich.panel import Panel
from rich.text import Text


@frozen
class Title:
    text: str
    order: int = 1

    def __rich__(self) -> RenderableType:
        if self.order == 1:
            return Padding(Panel(self.text, expand=False, box=box.SQUARE), (2, 0, 0, 0))
        elif self.order == 2:
            return Padding(Text(self.text.upper(), style="underline"), (1, 0, 0, 2))
        elif self.order == 3:
            return Text("  ⮕ " + self.text.upper())
        else:
            raise ValueError(f"Title order must be 1, 2, or 3, not {self.order}")


class AutoIndentingConsole(Console):
    """
    This custom console keeps track of the last title printed,
    and automatically indents the next line
    according to what level title we're under.
    """

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        self.active_title: Optional[Title] = None
        super().__init__(*args, markup=False, **kwargs)

    @property
    def auto_indent_size(self) -> int:
        if self.active_title is None:
            return 0

        if self.active_title.order == 1:
            return 2
        elif self.active_title.order == 2:
            return 2
        elif self.active_title.order == 3:
            return 5
        else:
            return 0

    def reset_title(self, order: int = 1) -> None:
        self.active_title = Title("", order=order)

    @staticmethod
    def extract_title(current: Optional[RenderableType]) -> Optional[Title]:
        while isinstance(current, Title) or hasattr(current, "renderable"):
            if isinstance(current, Title):
                return current
            current = getattr(current, "renderable", None)

        return None

    def print(self, *args: Any, deindent: int = 0, **kwargs: Any) -> None:
        """Override the default print.

        If what we're printing is a title, we remember it.
        If we remember a recent title,
        indent whatever we print now by how much the most recent title prefers.
        """
        indent = 0

        for arg in args:
            title = self.extract_title(arg)
            if title is not None:
                self.active_title = title
                break
        else:
            indent = self.auto_indent_size

        indent = max(0, indent - deindent)

        super().print(Padding.indent(Group(*args), indent), **kwargs)


MAX_WIDTH = 120
MIN_WIDTH = 40
terminal_width = get_terminal_size((MAX_WIDTH, 1))[0]
safe_width = min(MAX_WIDTH, terminal_width) if terminal_width > MIN_WIDTH else MIN_WIDTH
console = AutoIndentingConsole(highlighter=None, width=safe_width)
