"""
Save information about the command
"""
from attr import define
from typing_extensions import Literal
from typing_extensions import TypedDict


class CommandSchema(TypedDict, total=False):
    subcommand: str


CommandKeys = Literal["subcommand"]

DEFAULT_SETTINGS: CommandSchema = {"subcommand": "unset"}


@define
class Command:
    _contents: CommandSchema = DEFAULT_SETTINGS

    def set_subcommand(self, subcommand: str) -> None:
        self._contents["subcommand"] = subcommand

    def get_subcommand(self) -> str:
        return self._contents["subcommand"]
