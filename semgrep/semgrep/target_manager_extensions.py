from typing import Any
from typing import cast
from typing import Dict
from typing import List
from typing import NewType
from typing import Set
from typing import Tuple
from typing import TypeVar

from semgrep.error import _UnknownExtensionError
from semgrep.error import _UnknownLanguageError
from semgrep.semgrep_types import GENERIC_LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import REGEX_ONLY_LANGUAGE_KEYS


FileExtension = NewType("FileExtension", str)

# coupling: if you add a constant here, modify also ALL_EXTENSIONS below
# and you probably also need to update _LANGS_TO_EXTS_INTERNAL
PYTHON_EXTENSIONS = [FileExtension(".py"), FileExtension(".pyi")]
JAVASCRIPT_EXTENSIONS = [FileExtension(".js"), FileExtension(".jsx")]
TYPESCRIPT_EXTENSIONS = [FileExtension(".ts"), FileExtension(".tsx")]
JAVA_EXTENSIONS = [FileExtension(".java")]
C_EXTENSIONS = [FileExtension(".c")]
GO_EXTENSIONS = [FileExtension(".go")]
RUBY_EXTENSIONS = [FileExtension(".rb")]
PHP_EXTENSIONS = [FileExtension(".php")]
LUA_EXTENSIONS = [FileExtension(".lua")]
CSHARP_EXTENSIONS = [FileExtension(".cs")]
ML_EXTENSIONS = [
    FileExtension(".mli"),
    FileExtension(".ml"),
]
JSON_EXTENSIONS = [FileExtension(".json")]

# This is used to determine the set of files with known extensions,
# i.e. those for which we have a proper parser.
ALL_EXTENSIONS = (
    PYTHON_EXTENSIONS
    + JAVASCRIPT_EXTENSIONS
    + TYPESCRIPT_EXTENSIONS
    + JAVA_EXTENSIONS
    + C_EXTENSIONS
    + GO_EXTENSIONS
    + RUBY_EXTENSIONS
    + ML_EXTENSIONS
    + JSON_EXTENSIONS
)

# This is used to select the files suitable for spacegrep, which is
# all of them. It is spacegrep itself that will detect and ignore binary
# files.
GENERIC_EXTENSIONS = [FileExtension("")]

T = TypeVar("T")


def langauge_set(s: Set[str]) -> Set[Language]:
    # hack to make mypy accept our Set[str] as a set of Language
    return s  # type:ignore


# don't use this directly because it won't be O(1) lookup
# it's just for human readability, we process it below
_LANGS_TO_EXTS_INTERNAL: List[Tuple[Set[Language], List[FileExtension]]] = [
    (langauge_set({"python", "python2", "python3", "py"}), PYTHON_EXTENSIONS),
    (langauge_set({"js", "jsx", "javascript"}), JAVASCRIPT_EXTENSIONS),
    (langauge_set({"ts", "tsx", "typescript"}), TYPESCRIPT_EXTENSIONS),
    (langauge_set({"java"}), JAVA_EXTENSIONS),
    (langauge_set({"c"}), C_EXTENSIONS),
    (langauge_set({"go", "golang"}), GO_EXTENSIONS),
    (langauge_set({"ml", "ocaml"}), ML_EXTENSIONS),
    (langauge_set({"rb", "ruby"}), RUBY_EXTENSIONS),
    (langauge_set({"php"}), PHP_EXTENSIONS),
    (langauge_set({"json", "JSON", "Json"}), JSON_EXTENSIONS),
    (langauge_set({"lua"}), LUA_EXTENSIONS),
    (langauge_set({"cs", "csharp", "C#"}), CSHARP_EXTENSIONS),
    (REGEX_ONLY_LANGUAGE_KEYS.union({GENERIC_LANGUAGE}), GENERIC_EXTENSIONS),
]

# create a dictionary for fast lookup and reverse lookup
_LANGS_TO_EXTS: Dict[Language, List[FileExtension]] = {}
_EXTS_TO_LANGS: Dict[FileExtension, List[Language]] = {}
for language_set, extensions in _LANGS_TO_EXTS_INTERNAL:
    for lang in language_set:
        _LANGS_TO_EXTS[lang] = extensions
    for extension in extensions:
        _EXTS_TO_LANGS[extension] = list(language_set)


def ext_to_langs(ext: FileExtension) -> List[Language]:
    langs = _EXTS_TO_LANGS.get(ext)
    if langs is None:
        raise _UnknownExtensionError(f"Unsupported extension: {ext}")
    return langs


def lang_to_exts(language: Language) -> List[FileExtension]:
    """
    Convert language to expected file extensions

    If language is not a supported semgrep language then
    raises _UnknownLanguageError
    """
    extensions = _LANGS_TO_EXTS.get(language)
    if extensions is None:
        raise _UnknownLanguageError(f"Unsupported Language: {language}")
    return extensions
