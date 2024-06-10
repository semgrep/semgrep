import json
from pathlib import Path
from typing import Collection
from typing import Mapping
from typing import NewType
from typing import Optional
from uuid import UUID
from uuid import uuid4

from attrs import frozen

from semgrep.error import UnknownLanguageError
from semgrep.error_location import Span
from semgrep.types import JsonObject

Mode = NewType("Mode", str)
FileExtension = NewType("FileExtension", str)
Shebang = str

JOIN_MODE = Mode("join")
SEARCH_MODE = DEFAULT_MODE = Mode("search")
FROZEN_ID: UUID = uuid4()


def get_frozen_id() -> UUID:
    """
    Return a frozen UUID to identify a local scan and its corresponding results (if any)
    within the Semgrep App. We avoid initializing this UUID within our state object to
    prevent circular dependencies.
    """
    return FROZEN_ID


class Language(str):
    @property
    def definition(self) -> "LanguageDefinition":
        return LANGUAGE.definition_by_id[self]


@frozen
class LanguageDefinition:
    """
    Mirrors schema of lang.json (see semgrep_interfaces/README.md) for each language
    """

    id: Language
    name: str
    keys: Collection[str]
    exts: Collection[FileExtension]
    reverse_exts: Collection[str]
    shebangs: Collection[Shebang]
    is_target_language: bool

    @classmethod
    def from_dict(cls, data: JsonObject) -> "LanguageDefinition":
        # Assume all the fields exist in lang.json, which is generated.
        # Optional fields may be set to 'null'.
        return cls(
            id=Language(data["id"]),
            name=data["name"],
            keys=data["keys"],
            exts=data["exts"],
            reverse_exts=data["reverse_exts"]
            if data["reverse_exts"] is not None
            else data["exts"],
            shebangs=data.get("shebangs", []),
            is_target_language=data["is_target_language"],
        )


class _LanguageData:
    def __init__(self) -> None:
        with (Path(__file__).parent / "semgrep_interfaces" / "lang.json").open() as fd:
            data = json.load(fd)

        self.definition_by_id: Mapping[Language, LanguageDefinition] = {
            Language(d["id"]): LanguageDefinition.from_dict(d) for d in data
        }
        self.lang_by_key: Mapping[str, Language] = {
            key.lower(): lang
            for lang, definition in self.definition_by_id.items()
            for key in definition.keys
        }
        self.lang_by_ext: Mapping[str, Language] = {
            ext: lang
            for lang, definition in self.definition_by_id.items()
            for ext in definition.reverse_exts
        }
        self.all_language_keys: Collection[str] = sorted(self.lang_by_key.keys())

    def resolve(self, lang_str: str, span: Optional[Span] = None) -> Language:
        """
        Convert an inputted string representing a language to a Language

        :param lang_str: string representing a language (e.g. "C#")
        :param span: span of language string in the original file (for error reporting),
                    None if resolve was called within semgrep
        """
        normalized = lang_str.lower()
        if normalized in self.lang_by_key:
            return self.lang_by_key[normalized]
        else:
            spans = [span.with_context(before=1, after=1)] if span else []
            raise UnknownLanguageError(
                short_msg=f"invalid language: {normalized}",
                long_msg=f"unsupported language: {normalized}. {self.show_suppported_languages_message()}\n\nYou may need to update your version of Semgrep, if you are on an old version that does not yet support this language.",
                spans=spans,
            )

    def show_suppported_languages_message(self) -> str:
        languages_usable_in_the_languages_field = sorted(
            {
                key: value
                # "generic", "regex", and "none" are the only non-languages
                # that are supported in the "languages" field for historical reasons.
                for key, value in self.lang_by_key.items()
                if key not in ["spacegrep", "aliengrep"]
            }
        )
        return f"supported languages are: {', '.join(languages_usable_in_the_languages_field)}"


LANGUAGE = _LanguageData()

ALLOWED_GLOB_TYPES = ("include", "exclude")
