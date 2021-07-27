from typing import Dict
from typing import List

from semgrep.error import _UnknownExtensionError
from semgrep.error import _UnknownLanguageError
from semgrep.semgrep_types import FileExtension
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import Language_util


# coupling: if you add a constant here, modify also ALL_EXTENSIONS below
# and you probably also need to update semgrep_types.py Language
# and Language_util classes.
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
SCALA_EXTENSIONS = [FileExtension(".scala")]
VUE_EXTENSIONS = [FileExtension(".vue")]
HTML_EXTENSIONS = [FileExtension(".html"), FileExtension(".html")]

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
    + SCALA_EXTENSIONS
    + VUE_EXTENSIONS
    + HTML_EXTENSIONS
)

# This is used to select the files suitable for spacegrep, which is
# all of them. It is spacegrep itself that will detect and ignore binary
# files.
GENERIC_EXTENSIONS = [FileExtension("")]

_LANGS_TO_EXTS: Dict[Language, List[FileExtension]] = {
    Language.PYTHON: PYTHON_EXTENSIONS,
    Language.PYTHON2: PYTHON_EXTENSIONS,
    Language.PYTHON3: PYTHON_EXTENSIONS,
    Language.JAVASCRIPT: JAVASCRIPT_EXTENSIONS,
    Language.TYPESCRIPT: TYPESCRIPT_EXTENSIONS,
    Language.JAVA: JAVA_EXTENSIONS,
    Language.C: C_EXTENSIONS,
    Language.GO: GO_EXTENSIONS,
    Language.ML: ML_EXTENSIONS,
    Language.RUBY: RUBY_EXTENSIONS,
    Language.PHP: PHP_EXTENSIONS,
    Language.JSON: JSON_EXTENSIONS,
    Language.LUA: LUA_EXTENSIONS,
    Language.CSHARP: CSHARP_EXTENSIONS,
    Language.RUST: RUST_EXTENSIONS,
    Language.KOTLIN: KOTLIN_EXTENSIONS,
    Language.YAML: YAML_EXTENSIONS,
    Language.REGEX: GENERIC_EXTENSIONS,
    Language.GENERIC: GENERIC_EXTENSIONS,
    Language.SCALA: SCALA_EXTENSIONS,
    Language.VUE: VUE_EXTENSIONS,
    Language.HTML: HTML_EXTENSIONS,
}


def all_supported_languages() -> List[str]:
    """
    We want the list of languages to be deterministic, so sort it
    """
    return Language_util.all_language_strs()


# create a dictionary for fast lookup and reverse lookup
_EXTS_TO_LANG: Dict[FileExtension, Language] = {}
for language in _LANGS_TO_EXTS.keys():
    for extension in _LANGS_TO_EXTS[language]:
        # When there are multiple languages for an extension,
        # take the first one
        if not extension in _EXTS_TO_LANG:
            _EXTS_TO_LANG[extension] = language


def ext_to_lang(ext: FileExtension) -> Language:
    langs = _EXTS_TO_LANG.get(ext)
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
