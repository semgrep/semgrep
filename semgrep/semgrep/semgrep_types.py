import functools
from enum import Enum
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import NewType
from typing import Optional

from semgrep.error import UnknownLanguageError
from semgrep.rule_lang import Span

Mode = NewType("Mode", str)
PatternId = NewType("PatternId", str)
FileExtension = NewType("FileExtension", str)

JOIN_MODE = Mode("join")
SEARCH_MODE = DEFAULT_MODE = Mode("search")


class Language(Enum):
    PYTHON: str = "python"
    PYTHON2: str = "python2"
    PYTHON3: str = "python3"
    JAVASCRIPT: str = "javascript"
    TYPESCRIPT: str = "typescript"
    JAVA: str = "java"
    C: str = "c"
    GO: str = "go"
    RUBY: str = "ruby"
    PHP: str = "php"
    LUA: str = "lua"
    CSHARP: str = "csharp"
    RUST: str = "rust"
    KOTLIN: str = "kt"
    YAML: str = "yaml"
    ML: str = "ml"
    SCALA: str = "scala"
    VUE: str = "vue"
    HTML: str = "html"
    JSON: str = "json"
    REGEX: str = "regex"
    GENERIC: str = "generic"


class Language_util:
    language_to_strs: Dict[Language, List[str]] = {
        Language.PYTHON: [Language.PYTHON.value, "py"],
        Language.PYTHON2: [Language.PYTHON2.value],
        Language.PYTHON3: [Language.PYTHON3.value],
        Language.JAVASCRIPT: [Language.JAVASCRIPT.value, "js"],
        Language.TYPESCRIPT: [Language.TYPESCRIPT.value, "ts"],
        Language.JAVA: [Language.JAVA.value],
        Language.C: [Language.C.value],
        Language.GO: [Language.GO.value, "golang"],
        Language.RUBY: [Language.RUBY.value, "rb"],
        Language.PHP: [Language.PHP.value],
        Language.LUA: [Language.LUA.value],
        Language.CSHARP: [Language.CSHARP.value, "cs", "C#"],
        Language.RUST: [Language.RUST.value, "Rust", "rs"],
        Language.KOTLIN: [Language.KOTLIN.value, "Kotlin", "kotlin"],
        Language.YAML: [Language.YAML.value, "Yaml"],
        Language.SCALA: [Language.SCALA.value],
        Language.VUE: [Language.VUE.value],
        Language.HTML: [Language.HTML.value],
        Language.ML: [Language.ML.value, "ocaml"],
        Language.JSON: [Language.JSON.value, "JSON", "Json"],
        Language.REGEX: [Language.REGEX.value, "none"],
        Language.GENERIC: [Language.GENERIC.value],
    }

    str_to_language = {}
    for language in language_to_strs.keys():
        for lang_str in language_to_strs[language]:
            str_to_language[lang_str] = language

    """ Convert an inputted string representing a language to a Language

    Keyword arguments;
    lang_str -- string representing a language (e.g. "C#")
    span     -- span of language string in the original file (for error reporting),
                None if resolve was called within semgrep
    """

    @classmethod
    def resolve(cls, lang_str: str, span: Optional[Span] = None) -> Language:
        if lang_str in cls.str_to_language:
            return cls.str_to_language[lang_str]
        else:
            raise UnknownLanguageError(
                short_msg=f"invalid language: {lang_str}",
                long_msg=f"unsupported language: {lang_str}. supported languages are: {', '.join(cls.all_language_strs())}",
                spans=[span.with_context(before=1, after=1)]
                if span
                else [],  # not called from a config file
            )

    @classmethod
    def all_language_strs(cls) -> List[str]:
        # sort to standardize because this is used in outputting methods
        return sorted(cls.str_to_language.keys())


ALLOWED_GLOB_TYPES = ("include", "exclude")


@functools.total_ordering
class Range(NamedTuple):
    start: int
    end: int
    metavariables: Mapping[str, Any]
    propagated_metavariables: Dict[str, Any] = {}

    def metavariables_match(self, rhs: "Range") -> bool:
        """
        Returns true if and only if all metavariables in both this and the other Range
        refer to the same variables (if variable nodes), in the same scope, or the same
        expressions (if expression nodes).

        That is, if two patterns define a "$X", and $X refers to a variable in one
        pattern, then $X must refer to the same variable in both patterns.
        """
        to_match = set(self.metavariables.keys()) & set(rhs.metavariables.keys())
        return all(
            self.metavariables[v]
            and rhs.metavariables[v]
            and self.metavariables[v] == rhs.metavariables[v]
            for v in to_match
        )

    def __repr__(self) -> str:
        return (
            f"<{self.__class__.__name__} "
            f"start={self.start} "
            f"end={self.end} "
            f"metavariables={self.metavariables} "
            f"propagated={self.propagated_metavariables}>"
        )

    def __hash__(self) -> int:
        return hash((self.start, self.end))

    def __eq__(self, rhs: Any) -> bool:
        if not isinstance(rhs, type(self)):
            return False

        return (
            self.start == rhs.start
            and self.end == rhs.end
            and self.metavariables_match(rhs)
        )

    def __lt__(self, rhs: Any) -> bool:
        if not isinstance(rhs, type(self)):
            return False

        diff_self = self.end - self.start
        diff_rhs = rhs.end - rhs.start

        if diff_self == diff_rhs:
            return self.start < rhs.start

        return diff_self < diff_rhs
