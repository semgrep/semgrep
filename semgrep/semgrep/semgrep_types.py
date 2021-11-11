from enum import Enum
from typing import Dict
from typing import List
from typing import NewType
from typing import Optional

from semgrep.error import UnknownLanguageError
from semgrep.rule_lang import Span

Mode = NewType("Mode", str)
FileExtension = NewType("FileExtension", str)
Shebang = str

JOIN_MODE = Mode("join")
SEARCH_MODE = DEFAULT_MODE = Mode("search")

# coupling: if you add a new language here, you probably need to modify
# also target_manager_extensions.py
class Language(Enum):
    PYTHON: str = "python"
    PYTHON2: str = "python2"
    PYTHON3: str = "python3"
    JAVASCRIPT: str = "javascript"
    TYPESCRIPT: str = "typescript"
    JAVA: str = "java"
    C: str = "c"
    CPP: str = "cpp"
    GO: str = "go"
    RUBY: str = "ruby"
    PHP: str = "php"
    HACK: str = "hack"
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
    HCL: str = "hcl"
    BASH: str = "bash"
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
        Language.CPP: [Language.CPP.value, "C++"],
        Language.GO: [Language.GO.value, "golang"],
        Language.RUBY: [Language.RUBY.value, "rb"],
        Language.PHP: [Language.PHP.value],
        Language.HACK: [Language.HACK.value, "hacklang"],
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
        Language.HCL: [Language.HCL.value, "tf", "terraform", "HCL"],
        Language.BASH: [Language.BASH.value, "sh"],
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
