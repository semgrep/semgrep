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
#
# Extensions that are permissible for a given language. Semgrep-core will
# perform its own filtering to check that the file is indeed in the correct
# language before attempting full parsing. Additionally, all executable
# files without an extension will also be passed to semgrep-core.
#
CSHARP_EXTENSIONS = [FileExtension(".cs")]
C_EXTENSIONS = [FileExtension(".c")]
GO_EXTENSIONS = [FileExtension(".go")]
HACK_EXTENSIONS = [
    FileExtension(".hack"),
    FileExtension(".hck"),
    FileExtension(".hh"),
    FileExtension(".php"),
]
HCL_EXTENSIONS = [FileExtension(".tf")]
HTML_EXTENSIONS = [FileExtension(".html"), FileExtension(".html")]
JAVASCRIPT_EXTENSIONS = [FileExtension(".js"), FileExtension(".jsx")]
JAVA_EXTENSIONS = [FileExtension(".java")]
JSON_EXTENSIONS = [FileExtension(".json")]
KOTLIN_EXTENSIONS = [FileExtension(".kt"), FileExtension(".kts"), FileExtension(".ktm")]
LUA_EXTENSIONS = [FileExtension(".lua")]
ML_EXTENSIONS = [FileExtension(".mli"), FileExtension(".ml")]
PHP_EXTENSIONS = [FileExtension(".php")]
PYTHON_EXTENSIONS = [FileExtension(".py"), FileExtension(".pyi")]
RUBY_EXTENSIONS = [FileExtension(".rb")]
RUST_EXTENSIONS = [FileExtension(".rs")]
SCALA_EXTENSIONS = [FileExtension(".scala")]
TYPESCRIPT_EXTENSIONS = [FileExtension(".ts"), FileExtension(".tsx")]
VUE_EXTENSIONS = [FileExtension(".vue")]
YAML_EXTENSIONS = [FileExtension(".yaml"), FileExtension(".yml")]

# This is used to determine the set of files with known extensions,
# i.e. those for which we have a proper parser.
ALL_EXTENSIONS = (
    C_EXTENSIONS
    + GO_EXTENSIONS
    + HACK_EXTENSIONS
    + HCL_EXTENSIONS
    + HTML_EXTENSIONS
    + JAVASCRIPT_EXTENSIONS
    + JAVA_EXTENSIONS
    + JSON_EXTENSIONS
    + KOTLIN_EXTENSIONS
    + ML_EXTENSIONS
    + PYTHON_EXTENSIONS
    + RUBY_EXTENSIONS
    + RUST_EXTENSIONS
    + SCALA_EXTENSIONS
    + TYPESCRIPT_EXTENSIONS
    + VUE_EXTENSIONS
    + YAML_EXTENSIONS
)

# This is used to select the files suitable for spacegrep, which is
# all of them. It is spacegrep itself that will detect and ignore binary
# files.
GENERIC_EXTENSIONS = [FileExtension("")]

_LANGS_TO_EXTS: Dict[Language, List[FileExtension]] = {
    Language.C: C_EXTENSIONS,
    Language.CSHARP: CSHARP_EXTENSIONS,
    Language.GENERIC: GENERIC_EXTENSIONS,
    Language.GO: GO_EXTENSIONS,
    Language.HACK: HACK_EXTENSIONS,
    Language.HCL: HCL_EXTENSIONS,
    Language.HTML: HTML_EXTENSIONS,
    Language.JAVA: JAVA_EXTENSIONS,
    Language.JAVASCRIPT: JAVASCRIPT_EXTENSIONS,
    Language.JSON: JSON_EXTENSIONS,
    Language.KOTLIN: KOTLIN_EXTENSIONS,
    Language.LUA: LUA_EXTENSIONS,
    Language.ML: ML_EXTENSIONS,
    Language.PHP: PHP_EXTENSIONS,
    Language.PYTHON2: PYTHON_EXTENSIONS,
    Language.PYTHON3: PYTHON_EXTENSIONS,
    Language.PYTHON: PYTHON_EXTENSIONS,
    Language.REGEX: GENERIC_EXTENSIONS,
    Language.RUBY: RUBY_EXTENSIONS,
    Language.RUST: RUST_EXTENSIONS,
    Language.SCALA: SCALA_EXTENSIONS,
    Language.TYPESCRIPT: TYPESCRIPT_EXTENSIONS,
    Language.VUE: VUE_EXTENSIONS,
    Language.YAML: YAML_EXTENSIONS,
}

#
# Define for which languages we allow executable, extensionless files to
# be passed to semgrep-core. Semgrep-core will inspect the beginning of
# the file to determine if it's the requested language.
#
# Language.GENERIC is special. Semgrep-core will accept all the files if
# the language is 'generic'.
#
_LANG_IS_SCRIPT: Dict[Language, bool] = {
    Language.C: False,
    Language.CSHARP: False,
    Language.GENERIC: False,  # important/special
    Language.GO: False,
    Language.HACK: True,
    Language.HTML: False,
    Language.JAVA: False,
    Language.JAVASCRIPT: True,
    Language.JSON: False,
    Language.KOTLIN: False,
    Language.LUA: True,
    Language.ML: True,
    Language.PHP: True,
    Language.PYTHON2: True,
    Language.PYTHON3: True,
    Language.PYTHON: True,
    Language.REGEX: False,  # special
    Language.RUBY: True,
    Language.RUST: True,
    Language.SCALA: True,
    Language.TYPESCRIPT: True,
    Language.VUE: False,
    Language.YAML: False,
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
        raise _UnknownLanguageError(f"Unsupported language: {language}")
    return extensions


def lang_is_script(language: Language) -> bool:
    is_script = _LANG_IS_SCRIPT.get(language)
    if is_script is None:
        raise _UnknownLanguageError(
            f"Unsupported language: {language} (allows executable scripts?)"
        )
    return is_script
