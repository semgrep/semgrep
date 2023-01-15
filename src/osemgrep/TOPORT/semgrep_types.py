from semgrep.error import UnknownLanguageError
from semgrep.rule_lang import Span
from semgrep.types import JsonObject

FileExtension = NewType("FileExtension", str)
Shebang = str

# TODO: use ATD if really Lang.ml.j2 is not enough and we
# need to access more info from lang.json
class LanguageDefinition:
    id: Language
    name: str
    keys: Collection[str]
    exts: Collection[FileExtension]
    reverse_exts: Collection[str]
    shebangs: Collection[Shebang]

    @classmethod
    def from_dict(cls, data: JsonObject) -> "LanguageDefinition":
        return cls(
            id=Language(data["id"]),
            name=data["name"],
            keys=data["keys"],
            exts=data["exts"],
            reverse_exts=data.get("reverse_exts", data["exts"]),
            shebangs=data.get("shebangs", []),
        )


class _LanguageData:
    def __init__(self) -> None:
        with (Path(__file__).parent / "lang" / "lang.json").open() as fd:
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
                long_msg=f"unsupported language: {normalized}. {self.show_suppported_languages_message()}",
                spans=spans,
            )

ALLOWED_GLOB_TYPES = ("include", "exclude")
