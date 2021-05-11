from typing import Dict
from typing import List
from typing import Tuple

from semgrep.error import _UnknownExtensionError
from semgrep.error import _UnknownLanguageError
from semgrep.semgrep_types import FileExtension
from semgrep.semgrep_types import Language


# coupling: if you add a constant here, modify also ALL_EXTENSIONS below
# and you probably also need to update _LANGS_TO_EXTS_INTERNAL
# You may also have to regenerate some test snapshots with
# pipenv run pytest tests/e2e/test_rule_parser.py --snapshot-update
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
RUST_EXTENSIONS = [FileExtension(".rs")]
KOTLIN_EXTENSIONS = [FileExtension(".kt"), FileExtension(".kts"), FileExtension(".ktm")]
YAML_EXTENSIONS = [FileExtension(".yaml"), FileExtension(".yml")]
ML_EXTENSIONS = [FileExtension(".mli"), FileExtension(".ml")]
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
    + RUST_EXTENSIONS
    + KOTLIN_EXTENSIONS
    + YAML_EXTENSIONS
)

# This is used to select the files suitable for spacegrep, which is
# all of them. It is spacegrep itself that will detect and ignore binary
# files.
GENERIC_EXTENSIONS = [FileExtension("")]

PYTHON_LANGUAGES = [
    Language("python"),
    Language("python2"),
    Language("python3"),
    Language("py"),
]
JAVASCRIPT_LANGUAGES = [Language("javascript"), Language("js")]
TYPESCRIPT_LANGUAGES = [Language("typescript"), Language("ts")]
JAVA_LANGUAGES = [Language("java")]
C_LANGUAGES = [Language("c")]
GO_LANGUAGES = [Language("golang"), Language("go")]
RUBY_LANGUAGES = [Language("ruby"), Language("rb")]
PHP_LANGUAGES = [Language("php")]
LUA_LANGUAGES = [Language("lua")]
CSHARP_LANGUAGES = [Language("cs"), Language("csharp"), Language("C#")]
RUST_LANGUAGES = [Language("rust"), Language("Rust"), Language("rs")]
KOTLIN_LANGUAGES = [Language("kotlin"), Language("Kotlin"), Language("kt")]
YAML_LANGUAGES = [Language("yaml"), Language("Yaml")]
ML_LANGUAGES = [Language("ocaml"), Language("ml")]
JSON_LANGUAGES = [Language("json"), Language("JSON"), Language("Json")]
REGEX_LANGUAGES = [Language("none"), Language("regex")]
GENERIC_LANGUAGES = [Language("generic")]


# don't use this directly because it won't be O(1) lookup
# it's just for human readability, we process it below
_LANGS_TO_EXTS_INTERNAL: List[Tuple[List[Language], List[FileExtension]]] = [
    (PYTHON_LANGUAGES, PYTHON_EXTENSIONS),
    (JAVASCRIPT_LANGUAGES, JAVASCRIPT_EXTENSIONS),
    (TYPESCRIPT_LANGUAGES, TYPESCRIPT_EXTENSIONS),
    (JAVA_LANGUAGES, JAVA_EXTENSIONS),
    (C_LANGUAGES, C_EXTENSIONS),
    (GO_LANGUAGES, GO_EXTENSIONS),
    (ML_LANGUAGES, ML_EXTENSIONS),
    (RUBY_LANGUAGES, RUBY_EXTENSIONS),
    (PHP_LANGUAGES, PHP_EXTENSIONS),
    (JSON_LANGUAGES, JSON_EXTENSIONS),
    (LUA_LANGUAGES, LUA_EXTENSIONS),
    (CSHARP_LANGUAGES, CSHARP_EXTENSIONS),
    (RUST_LANGUAGES, RUST_EXTENSIONS),
    (KOTLIN_LANGUAGES, KOTLIN_EXTENSIONS),
    (YAML_LANGUAGES, YAML_EXTENSIONS),
    (REGEX_LANGUAGES + GENERIC_LANGUAGES, GENERIC_EXTENSIONS),
]


def all_supported_languages() -> List[Language]:
    """
    We want the list of languages to be deterministic, so sort it
    """
    return sorted(sum((languages for languages, _ in _LANGS_TO_EXTS_INTERNAL), []))


# create a dictionary for fast lookup and reverse lookup
_LANGS_TO_EXTS: Dict[Language, List[FileExtension]] = {}
_EXTS_TO_LANGS: Dict[FileExtension, List[Language]] = {}
for languages, extensions in _LANGS_TO_EXTS_INTERNAL:
    for lang in languages:
        _LANGS_TO_EXTS[lang] = extensions
    for extension in extensions:
        _EXTS_TO_LANGS[extension] = languages


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
