# A semi-automated generated Pydantic model to validate our Rule schema.
#
# @author: zz@semgrep.com
# @date: 2024-08-20
#
# This model is generated from the rule_schema_v1.yaml file in the semgrep repository,
# and then tuned with some additional custom logic to smooth over some rough edges.
#
# For generating this model, we used https://docs.pydantic.dev/latest/integrations/datamodel_code_generator/
# whose homepage is https://koxudaxi.github.io/datamodel-code-generator/
#
# Ideally running
# $ datamodel-codegen --input interfaces/semgrep-interfaces/rule_schema_v1.yaml --input-file-type jsonschema --output model.py
# would just work out of the box, but there a few issues that require manual intervention.
#
# Steps used to generate this model:
#
# 1. The yaml file first needs to be converted to jsonschema format which can be done using the following command:
#    $ python -c 'import yaml, json; print(json.dumps(yaml.safe_load(open("interfaces/semgrep-interfaces/rule_schema_v1.yaml")), indent=2))' > rule_schema_v1.json
# 2. Now we can run `$ datamodel-codegen --input rule_schema_v1.json --input-file-type jsonschema --output model.py`
# 3. Next, we need to update the generated model to pydantic v2 (see https://docs.pydantic.dev/latest/migration/#migration-guide) which is a
#    somewhat labor intensive and manual process. The generated model will also a lot duplicate class definitions which should be cleaned up.
#    This will manifest as a lot of `class NewPatternX(BaseModel)` with X being a number from 1 to 8.
# 4. Sadly, we need to update the `Languages` enum with the extended `lang.json` values and add some custom python code to handle extended matches
#    for the `Languages` enum.
# 5. Then, there's some custom logic for validation (e.g. `model_validator` and `field_validator` functions) that needs to be added to the model
#    as well as `pattern_discriminator` and `focus_discriminator` functions for better error messages, including the `check_language_static` to validate.
# 6. Finally, there's a bit of tuning that I undertook and hopefully we won't need to do again once we move validation directly in OCaml.
#
#
from enum import Enum
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Union

from pydantic import BaseModel
from pydantic import ConfigDict
from pydantic import Discriminator
from pydantic import Field
from pydantic import field_validator
from pydantic import model_validator
from pydantic import RootModel
from pydantic import Tag
from pydantic_core import PydanticCustomError
from typing_extensions import Annotated


class RuleMode(str, Enum):
    """
    NOTE: Ensure that we also inherit from str to prevent footguns when comparing equality with raw strings
    """

    search = "search"
    taint = "taint"
    _join = "join"  # NOTE: This is a workaround to avoid a name conflict with the str `join` field
    extract = "extract"


class JoinedRuleMode(str, Enum):
    """
    NOTE: While this enum does not exist in the original rule schema, we split the `RuleMode` enum into two separate enums
    for clarity of the expected options for the `mode` field in the `JoinedRules` model.
    """

    search = "search"
    taint = "taint"


class Languages(str, Enum):
    """
    NOTE: Derived from `lang.json` as the `languages` definition in `rule_schema_v1.yaml`
    is not the source of truth for the languages that Semgrep supports.

    To create this enum, we can follow the following steps:

    1. Run the following command to extract the language keys from lang.json:
        $ jq -r '. | map(.keys[]) | sort' interfaces/semgrep-interfaces/lang.json
    2. Copy the output which should look like this:
    [
        "aliengrep",
        "apex",
        "bash",
        "c",
        "c#",
        "c++",
        ...
    ]
    3. Paste the output into an IDE and format the data to `{lang} = "{lang}"` for each language key
    4. Adust the key values for languages that are not valid Python identifiers (e.g. `c#` -> `_cs`)
    5. Update the `Languages` enum below with the formatted data
    """

    aliengrep = "aliengrep"
    apex = "apex"
    bash = "bash"
    c = "c"
    _cs = "c#"
    _cpp = "c++"
    cairo = "cairo"
    circom = "circom"
    clojure = "clojure"
    cpp = "cpp"
    csharp = "csharp"
    dart = "dart"
    docker = "docker"
    dockerfile = "dockerfile"
    elixir = "elixir"
    ex = "ex"
    generic = "generic"
    go = "go"
    golang = "golang"
    hack = "hack"
    hcl = "hcl"
    html = "html"
    java = "java"
    javascript = "javascript"
    js = "js"
    json = "json"
    jsonnet = "jsonnet"
    julia = "julia"
    kotlin = "kotlin"
    kt = "kt"
    lisp = "lisp"
    lua = "lua"
    move_on_aptos = "move_on_aptos"
    none = "none"
    ocaml = "ocaml"
    php = "php"
    promql = "promql"
    proto = "proto"
    proto3 = "proto3"
    protobuf = "protobuf"
    py = "py"
    python = "python"
    python2 = "python2"
    python3 = "python3"
    ql = "ql"
    r = "r"
    regex = "regex"
    ruby = "ruby"
    rust = "rust"
    scala = "scala"
    scheme = "scheme"
    sh = "sh"
    sol = "sol"
    solidity = "solidity"
    spacegrep = "spacegrep"
    swift = "swift"
    terraform = "terraform"
    tf = "tf"
    ts = "ts"
    typescript = "typescript"
    vue = "vue"
    xml = "xml"
    yaml = "yaml"

    @classmethod
    def _missing_(cls, value: object) -> Union["Languages", None]:
        """
        Instead of defining values for both "C#" and "c#" in the enum to account for case-insensitive matching,
        we can use this built-in method to resolve enum members that otherwise wouldn't be found through the normal
        lookup process. Note that pydantic field validation will respect this method when resolving enum members.

        See https://docs.python.org/3/library/enum.html#enum.Enum._missing_
        """
        search_value = value.lower() if isinstance(value, str) else value
        return next((member for member in cls if search_value == member.value), None)

    @classmethod
    def resolve(cls, value: str) -> Union["Languages", None]:
        """
        Resolve a raw language string value to the correct enum member or None.

        This method will use the `_missing_` method under the hood and is useful for checking membership
        of the `Languages` enum or comparing against the `_LanguageData` class.

        Note that we can't use `hasattr(Languages, value)` for all cases, as `c#` for example, is not a
        valid Python identifier. Instead, we can use this method to resolve the value to the correct enum
        (or None) and take full advantage of the `_missing_` method to handle case-insensitive matching.
        """
        try:
            return cls(value)
        except ValueError:
            return None


def check_language_static(v: Union[str, List[str]]) -> Union[str, List[str]]:
    """
    While pydantic will run a validation of the input against the members of the `Languages` enum,
    we can provide a better error message that calls out the invalid language(s) at the beginning of
    the exception for inputs of multiple languages. This method will be called prior to the pydantic
    validation.

    Note that for inputs of a single language, we can just return the input unchanged and pydantic will
    validation and a suitable error message without any need for customizations.
    """
    if isinstance(v, list):
        # Check for singleton list and return the string for a better error message from pydantic
        if len(v) == 1:
            return v[0]
        # Otherwise, check for invalid languages via the `resolve` method
        _invalid_langs = [lang for lang in v if not (Languages.resolve(lang))]
        # If there's only one invalid language, return it as a string for a better error message
        invalid_langs = (
            _invalid_langs[0]
            if _invalid_langs and len(_invalid_langs) == 1
            else _invalid_langs
        )
        if invalid_langs:
            valid_langs = [f"'{lang.value}'" for lang in Languages]
            if len(valid_langs) > 1:  # Add an "or" to the last valid language
                valid_langs[-1] = f"or {valid_langs[-1]}"
            expected_msg = f"Expected one of {', '.join(lang for lang in valid_langs)}"
            raise ValueError(f"Invalid languages `{invalid_langs}`, {expected_msg}")
    return v


class Severity(str, Enum):
    ERROR = "ERROR"
    WARNING = "WARNING"
    INFO = "INFO"
    # EXPERIMENTAL
    INVENTORY = "INVENTORY"
    EXPERIMENT = "EXPERIMENT"
    # since 1.72.0
    CRITICAL = "CRITICAL"
    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    LOW = "LOW"


class FixRegex(BaseModel):
    model_config = ConfigDict(extra="forbid")

    count: Optional[int] = Field(None, title="Replace up to this many regex matches")
    regex: str = Field(..., title="Regular expression to find in matched code")
    replacement: str = Field(
        ...,
        title="Code to replace the regular expression match with. Can use capture groups.",
    )


class Reduce(str, Enum):
    concat = "concat"
    separate = "separate"


class Transform(str, Enum):
    no_transform = "no_transform"
    unquote_string = "unquote_string"
    concat_json_string_array = "concat_json_string_array"


class Header(BaseModel):
    name: Optional[str] = None
    value: Optional[str] = None


class HttpRequestContent(BaseModel):
    url: Optional[str] = None
    method: Optional[str] = None
    headers: Optional[Dict[str, Any]] = None
    body: Optional[str] = None
    auth: Optional[Dict[str, Any]] = None


class GeneralPatternContent(RootModel):
    root: Union[str, Mapping] = Field(
        ..., title="Return finding where code matches against the following pattern"
    )


class SecretValidity(str, Enum):
    valid = "valid"
    invalid = "invalid"


class HttpResult(BaseModel):
    validity: Optional[SecretValidity] = None


def http_status_discriminator(value: Any) -> Union[str, None]:
    if isinstance(value, str):
        return "str"
    elif isinstance(value, int):
        return "int"
    else:
        # NOTE: Pydantic will create a better error message for us
        return None


class HttpResponseMatch(BaseModel):
    """
    NOTE: Ideally we should only be using integers for status codes, but we
    are more permissive here as the original schema allows for strings.

    We can see that the `create_validator_rule` function in the `test_validator_rule_is_blocking`
    test of `test_rule.py` uses strings for status codes.
    """

    status_code: Annotated[
        Union[
            Annotated[str, Tag("str")],
            Annotated[int, Tag("int")],
        ],
        Discriminator(
            http_status_discriminator,
        ),
    ] = Field(None, alias="status-code")
    headers: Optional[List[Header]] = None
    content: Optional[GeneralPatternContent] = None


class HttpResponseItem(BaseModel):
    match: List[HttpResponseMatch]
    result: Optional[HttpResult] = None


class HttpResponseContent(RootModel):
    root: List[HttpResponseItem]


class Http(BaseModel):
    request: HttpRequestContent
    response: HttpResponseContent


class SecretValidator1(BaseModel):
    http: Http


class SecretValidator(RootModel):
    root: SecretValidator1


class WhereFocus(BaseModel):
    focus: Union[str, List[str]]


class WhereCompare(BaseModel):
    comparison: str
    base: Optional[int] = Field(
        None, title="Integer base to parse metavariable contents as"
    )
    strip: Optional[bool] = Field(
        None, title="Whether to strip quotes from compared metavariables"
    )


class WhereMeta(BaseModel):
    metavariable: str


class WhereMetaType(BaseModel):
    metavariable: str
    type: str


class WhereMetaTypes(BaseModel):
    metavariable: str
    types: List[str]


class WhereMetaAnalyzer(BaseModel):
    metavariable: str
    analyzer: str


class NewPattern(RootModel):
    root: Union[str, "NewPattern1"]


class NewPattern1(BaseModel):
    model_config = ConfigDict(extra="forbid")
    pattern: Optional[str] = None
    regex: Optional[str] = None
    all: Optional[List[NewPattern]] = Field(None, min_length=1)
    any: Optional[List[NewPattern]] = Field(None, min_length=1)
    not_: Optional[NewPattern] = Field(None, alias="not")
    inside: Optional[NewPattern] = None
    anywhere: Optional[NewPattern] = None
    where: Optional[
        List[
            Union[
                WhereFocus,
                WhereCompare,
                NewPattern,
                WhereMeta,
                WhereMetaType,
                WhereMetaTypes,
                WhereMetaAnalyzer,
            ]
        ]
    ] = None


class NewSourcePattern(RootModel):
    root: Union[str, "NewSourcePattern1"]


class NewSourcePattern1(NewPattern1):
    requires: Optional[str] = None
    label: Optional[str] = None


class NewSinkPattern(RootModel):
    root: Union[str, "NewSinkPattern1"]


class NewSinkPattern1(NewPattern1):
    requires: Optional[str] = None


class NewSanitizerPattern(RootModel):
    root: Union[str, "NewSanitizerPattern1"]


class NewSanitizerPattern1(NewPattern1):
    not_conflicting: Optional[str] = Field(None, alias="not-conflicting")


class NewPropagatorPattern(RootModel):
    root: Union[str, "NewPropagatorPattern1"]


class NewPropagatorPattern1(NewPattern1):
    from_: str = Field(..., alias="from")
    to: str


class NewTaintContent(BaseModel):
    sources: List[NewSourcePattern] = Field(..., min_length=1)
    sinks: List[NewSinkPattern] = Field(..., min_length=1)
    propagators: Optional[List[NewPropagatorPattern]] = Field(None, min_length=1)
    sanitizers: Optional[List[NewSanitizerPattern]] = Field(None, min_length=1)


class MetavariableComparison1(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: Optional[str] = Field(None, title="Metavariable to compare")
    comparison: str = Field(..., title="Comparison expression")
    strip: Optional[bool] = None
    base: Optional[int] = None


class MetavariableComparison(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable_comparison: MetavariableComparison1 = Field(
        ...,
        alias="metavariable-comparison",
        title="Compare metavariables with other metavariables or literals using constant propagation",
    )


class Pattern(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern: str = Field(
        ..., title="Return finding where Semgrep pattern matches exactly"
    )


class PatternRegex(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern_regex: str = Field(
        ...,
        alias="pattern-regex",
        title="Return finding where regular expression matches",
    )


class PatternNotRegex(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern_not_regex: str = Field(
        ...,
        alias="pattern-not-regex",
        title="Do not return finding where regular expression matches",
    )


def focus_discriminator(value: Union[List[str], str]) -> Union[str, None]:
    if isinstance(value, list):
        return "List[str]"
    elif isinstance(value, str):
        return "str"
    else:
        # NOTE: Pydantic will create a better error message for us
        return None


class FocusMetavariable(BaseModel):
    model_config = ConfigDict(extra="forbid")

    focus_metavariable: Annotated[
        Union[
            Annotated[List[str], Tag("List[str]")],
            Annotated[str, Tag("str")],
        ],
        Discriminator(
            focus_discriminator,
        ),
    ] = Field(
        ...,
        alias="focus-metavariable",
        title="Focus on what a given metavariable is matching",
    )


class PatternInside(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern_inside: GeneralPatternContent = Field(
        ...,
        alias="pattern-inside",
        title="Return findings only from within snippets Semgrep pattern matches",
    )


class SemgrepInternalPatternAnywhere(BaseModel):
    model_config = ConfigDict(extra="forbid")

    semgrep_internal_pattern_anywhere: GeneralPatternContent = Field(
        ...,
        alias="semgrep-internal-pattern-anywhere",
        title="Marks this subpattern such that at a containing `patterns` or other form of conjunction the range is not considered; subpattern matches are instead combined solely on the basis of metavariables, without respect to range, and the range of the matching subpattern is discarded wholly.",
    )


class PatternNotInside(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern_not_inside: GeneralPatternContent = Field(
        ...,
        alias="pattern-not-inside",
        title="Do not return findings from within snippets Semgrep pattern matches",
    )


class PatternNot(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern_not: GeneralPatternContent = Field(
        ...,
        alias="pattern-not",
        title="Do not return finding where Semgrep pattern matches exactly",
    )


class PatternWherePython(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern_where_python: str = Field(
        ...,
        alias="pattern-where-python",
        title="Return finding where Python expression returns true",
    )


class Patterns(BaseModel):
    model_config = ConfigDict(extra="forbid")

    patterns: "PatternsContent" = Field(
        ..., title="Return finding where all of the nested conditions are true"
    )


class PatternEither(BaseModel):
    model_config = ConfigDict(extra="forbid")

    pattern_either: "PatternEitherContent" = Field(
        ...,
        alias="pattern-either",
        title="Return finding where any of the nested conditions are true",
    )


def pattern_either_discriminator(value: Any) -> Union[str, None]:
    if "patterns" in value:
        return "Patterns"
    elif "pattern-either" in value:
        return "PatternEither"
    elif "pattern-inside" in value:
        return "PatternInside"
    elif "semgrep-internal-pattern-anywhere" in value:
        return "SemgrepInternalPatternAnywhere"
    elif "pattern" in value:
        return "Pattern"
    elif "pattern-regex" in value:
        return "PatternRegex"
    else:
        # NOTE: Pydantic will create a better error message for us
        #      if we return None here instead of raising a ValueError
        return None


class PatternEitherContent(RootModel):
    root: List[
        Annotated[
            Union[
                Annotated["Patterns", Tag("Patterns")],
                Annotated["PatternEither", Tag("PatternEither")],
                Annotated["PatternInside", Tag("PatternInside")],
                Annotated[
                    "SemgrepInternalPatternAnywhere",
                    Tag("SemgrepInternalPatternAnywhere"),
                ],
                Annotated["Pattern", Tag("Pattern")],
                Annotated["PatternRegex", Tag("PatternRegex")],
            ],
            Discriminator(
                pattern_either_discriminator,
            ),
        ]
    ] = Field(..., title="Return finding where any of the nested conditions are true")


class MetavariablePattern1(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: str = Field(..., title="Metavariable to match")
    language: Optional[str] = None
    pattern: str = Field(
        ..., title="Return finding where Semgrep pattern matches exactly"
    )
    pattern_regex: Optional[str] = Field(
        None,
        alias="pattern-regex",
        title="Return finding where regular expression matches exactly",
    )
    patterns: Optional["PatternsContent"] = None
    pattern_either: Optional[PatternEitherContent] = Field(None, alias="pattern-either")


class MetavariablePattern2(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: str = Field(..., title="Metavariable to match")
    language: Optional[str] = None
    pattern: Optional[str] = Field(
        None, title="Return finding where Semgrep pattern matches exactly"
    )
    pattern_regex: Optional[str] = Field(
        None,
        alias="pattern-regex",
        title="Return finding where regular expression matches exactly",
    )
    patterns: "PatternsContent"
    pattern_either: Optional[PatternEitherContent] = Field(None, alias="pattern-either")


class MetavariablePattern3(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: str = Field(..., title="Metavariable to match")
    language: Optional[str] = None
    pattern: Optional[str] = Field(
        None, title="Return finding where Semgrep pattern matches exactly"
    )
    pattern_regex: Optional[str] = Field(
        None,
        alias="pattern-regex",
        title="Return finding where regular expression matches exactly",
    )
    patterns: Optional["PatternsContent"] = None
    pattern_either: PatternEitherContent = Field(..., alias="pattern-either")


class MetavariablePattern4(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: str = Field(..., title="Metavariable to match")
    language: Optional[str] = None
    pattern: Optional[str] = Field(
        None, title="Return finding where Semgrep pattern matches exactly"
    )
    pattern_regex: str = Field(
        ...,
        alias="pattern-regex",
        title="Return finding where regular expression matches exactly",
    )
    patterns: Optional["PatternsContent"] = None
    pattern_either: Optional[PatternEitherContent] = Field(None, alias="pattern-either")


class MetavariablePattern(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable_pattern: Union[
        MetavariablePattern1,
        MetavariablePattern2,
        MetavariablePattern3,
        MetavariablePattern4,
    ] = Field(
        ...,
        alias="metavariable-pattern",
        title="Match metavariable value with a pattern formula",
    )


class MetavariableAnalysis1(BaseModel):
    model_config = ConfigDict(extra="forbid")

    analyzer: str = Field(..., title="Analyzer to use")
    metavariable: str = Field(..., title="Metavariable to analyze")
    options: Optional[Dict[str, Any]] = None


class MetavariableAnalysis(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable_analysis: MetavariableAnalysis1 = Field(
        ...,
        alias="metavariable-analysis",
        title="Inspect a metavariable with a given analyzer",
    )


class MetavariableType1(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: Optional[str] = Field(None, title="Metavariable to match")
    type: str = Field(..., title="Type expression")
    types: Optional[List[str]] = Field(None, title="Type expressions")
    language: Optional[str] = None


class MetavariableType2(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: Optional[str] = Field(None, title="Metavariable to match")
    type: Optional[str] = Field(None, title="Type expression")
    types: List[str] = Field(..., title="Type expressions")
    language: Optional[str] = None


class MetavariableType(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable_type: Union[MetavariableType1, MetavariableType2] = Field(
        ...,
        alias="metavariable-type",
        title="Filter for metavariables with a certain type",
    )


class MetavariableRegex1(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable: str = Field(..., title="Metavariable to search")
    regex: str = Field(..., title="PCRE regular expression")
    constant_propagation: Optional[bool] = Field(None, alias="constant-propagation")


class MetavariableRegex(BaseModel):
    model_config = ConfigDict(extra="forbid")

    metavariable_regex: MetavariableRegex1 = Field(
        ..., alias="metavariable-regex", title="Search metavariable value with RegEx"
    )


class Rename(BaseModel):
    from_: Optional[str] = Field(None, alias="from")
    to: Optional[str] = None


class Ref(BaseModel):
    model_config = ConfigDict(extra="forbid")

    rule: Optional[str] = None
    renames: Optional[List[Rename]] = None
    as_: Optional[str] = Field(None, alias="as")


class JoinContent(BaseModel):
    model_config = ConfigDict(extra="forbid")

    refs: Optional[List[Ref]] = None
    rules: Optional[List["JoinedRules"]] = None
    on: Optional[List[str]] = None
    additionalProperties: Optional[Any] = None


def pattern_discriminator(value: Any) -> Union[str, None]:
    if "pattern" in value:
        return "Pattern"
    elif "patterns" in value:
        return "Patterns"
    elif "pattern-either" in value:
        return "PatternEither"
    elif "focus-metavariable" in value:
        return "FocusMetavariable"
    elif "pattern-inside" in value:
        return "PatternInside"
    elif "semgrep-internal-pattern-anywhere" in value:
        return "SemgrepInternalPatternAnywhere"
    elif "pattern-not-inside" in value:
        return "PatternNotInside"
    elif "pattern-not" in value:
        return "PatternNot"
    elif "pattern-regex" in value:
        return "PatternRegex"
    elif "pattern-not-regex" in value:
        return "PatternNotRegex"
    elif "metavariable-analysis" in value:
        return "MetavariableAnalysis"
    elif "metavariable-regex" in value:
        return "MetavariableRegex"
    elif "metavariable-pattern" in value:
        return "MetavariablePattern"
    elif "metavariable-type" in value:
        return "MetavariableType"
    elif "metavariable-comparison" in value:
        return "MetavariableComparison"
    elif "pattern-where-python" in value:
        return "PatternWherePython"
    else:
        # NOTE: Pydantic will create a better error message for us
        #      if we return None here instead of raising a ValueError
        return None


class PatternsContent(RootModel):
    root: List[
        Annotated[
            Union[
                Annotated["Pattern", Tag("Pattern")],
                Annotated["Patterns", Tag("Patterns")],
                Annotated["PatternEither", Tag("PatternEither")],
                Annotated["FocusMetavariable", Tag("FocusMetavariable")],
                Annotated["PatternInside", Tag("PatternInside")],
                Annotated[
                    "SemgrepInternalPatternAnywhere",
                    Tag("SemgrepInternalPatternAnywhere"),
                ],
                Annotated["PatternNotInside", Tag("PatternNotInside")],
                Annotated["PatternNot", Tag("PatternNot")],
                Annotated["PatternRegex", Tag("PatternRegex")],
                Annotated["PatternNotRegex", Tag("PatternNotRegex")],
                Annotated["MetavariableAnalysis", Tag("MetavariableAnalysis")],
                Annotated["MetavariableRegex", Tag("MetavariableRegex")],
                Annotated["MetavariablePattern", Tag("MetavariablePattern")],
                Annotated["MetavariableType", Tag("MetavariableType")],
                Annotated["MetavariableComparison", Tag("MetavariableComparison")],
                Annotated["PatternWherePython", Tag("PatternWherePython")],
            ],
            Discriminator(
                pattern_discriminator,
            ),
        ]
    ] = Field(..., title="Return finding where all of the nested conditions are true")


class JoinedRules(BaseModel):
    model_config = ConfigDict(extra="forbid")

    id: "Id"
    languages: Optional[
        Annotated[Union[Languages, List[Languages]], Tag("languages")]
    ] = Field(None, title="Language")
    pattern: Optional[str] = Field(
        None, title="Return finding where Semgrep pattern matches exactly"
    )
    patterns: Optional[PatternsContent] = None
    mode: Optional[JoinedRuleMode] = None
    pattern_sources: Optional["TaintContent"] = Field(None, alias="pattern-sources")
    pattern_propagators: Optional["TaintContent"] = Field(
        None, alias="pattern-propagators"
    )
    pattern_sinks: Optional["TaintContent"] = Field(None, alias="pattern-sinks")
    pattern_sanitizers: Optional["TaintContent"] = Field(
        None, alias="pattern-sanitizers"
    )

    @field_validator("languages", mode="before")
    @classmethod
    def check_language(cls, v: Union[str, List[str]]) -> Union[str, List[str]]:
        return check_language_static(v)


class TaintContentPattern(BaseModel):
    pattern: str = Field(
        ..., title="Return finding where Semgrep pattern matches exactly"
    )


class TaintContentPatternsContent(BaseModel):
    patterns: PatternsContent = Field(
        ..., title="Return finding where all of the nested conditions are true"
    )


class TaintContentPatternEither(BaseModel):
    pattern_either: PatternEitherContent = Field(..., alias="pattern-either")


class TaintContentRegex(BaseModel):
    pattern_regex: str = Field(
        ...,
        alias="pattern-regex",
        title="Return finding where regular expression matches exactly",
    )


class TaintContent(RootModel):
    root: List[
        Union[
            TaintContentPattern,
            TaintContentPatternsContent,
            TaintContentPatternEither,
            TaintContentRegex,
        ]
    ]


class Id(RootModel):
    root: str = Field(
        pattern=r"^[a-zA-Z0-9._-]*$", title="Rule ID to attach to findings"
    )


class PathArray(RootModel):
    root: List[str]


class Paths(BaseModel):
    model_config = ConfigDict(extra="forbid")

    include: Optional[PathArray] = None
    exclude: Optional[PathArray] = None


class DestRules(BaseModel):
    include: Optional[PathArray] = None
    exclude: Optional[PathArray] = None


class DependsOnEither(BaseModel):
    model_config = ConfigDict(extra="forbid")

    namespace: str
    package: str
    version: str


# NOTE: This looks messed up as it's a union between a generic list and a dataclass
class R2cInternalProjectDependsOnContent1(BaseModel):
    namespace: str
    package: str
    version: str
    depends_on_either: Optional[Union[List, DependsOnEither]] = Field(
        None, alias="depends-on-either"
    )


class R2cInternalProjectDependsOnContent2(BaseModel):
    namespace: Optional[str] = None
    package: Optional[str] = None
    version: Optional[str] = None
    depends_on_either: Union[List, DependsOnEither] = Field(
        ..., alias="depends-on-either"
    )


class R2cInternalProjectDependsOnContent(RootModel):
    root: Union[
        R2cInternalProjectDependsOnContent1, R2cInternalProjectDependsOnContent2
    ] = Field(
        ..., title="One or more dependencies that the project contains in a lock file"
    )


class Rule(BaseModel):
    model_config = ConfigDict(extra="allow")

    id: Optional[Id] = None
    min_version: Optional[str] = Field(
        None, alias="min-version", title="Earliest Semgrep version supporting this rule"
    )
    max_version: Optional[str] = Field(
        None, alias="max-version", title="Last Semgrep version supporting this rule"
    )
    version: Optional[str] = Field(None, title="Version of rule")
    message: Optional[str] = Field(None, title="Description to attach to findings")
    mode: Optional[RuleMode] = Field(
        RuleMode.search, title="Mode of the rule"
    )  # NOTE: I don't quite follow why we have a default value here
    languages: Optional[
        Annotated[Union[Languages, List[Languages]], Tag("languages")]
    ] = Field(None, title="Language")
    paths: Optional[Paths] = Field(None, title="Path globs this pattern should run on")
    severity: Optional[Severity] = Field(
        None, title="Severity to report alongside this finding"
    )
    pattern_sinks: Optional[TaintContent] = Field(None, alias="pattern-sinks")
    pattern_sources: Optional[TaintContent] = Field(None, alias="pattern-sources")
    pattern_sanitizers: Optional[TaintContent] = Field(None, alias="pattern-sanitizers")
    pattern_propagators: Optional[TaintContent] = Field(
        None, alias="pattern-propagators"
    )
    taint: Optional[NewTaintContent] = None
    join: Optional[JoinContent] = None
    validators: Optional[List[SecretValidator]] = Field(None, min_length=1)
    fix: Optional[str] = Field(
        None,
        title="Replacement text to fix matched code. Can use matched metavariables.",
    )
    fix_regex: Optional[FixRegex] = Field(
        None, alias="fix-regex", title="Replacement regex to fix matched code."
    )
    metadata: Optional[Dict[str, Any]] = Field(
        None, title="Arbitrary structured data for your own reference"
    )
    options: Optional[Dict[str, Any]] = Field(
        None,
        title="Options object to enable/disable certain matching features in semgrep-core",
    )
    pattern: Optional[str] = Field(
        None, title="Return finding where Semgrep pattern matches exactly"
    )
    pattern_regex: Optional[str] = Field(
        None,
        alias="pattern-regex",
        title="Return finding where regular expression matches exactly",
    )
    patterns: Optional[PatternsContent] = None
    pattern_either: Optional[PatternEitherContent] = Field(None, alias="pattern-either")
    r2c_internal_project_depends_on: Optional[
        R2cInternalProjectDependsOnContent
    ] = Field(None, alias="r2c-internal-project-depends-on")
    match: Optional[NewPattern] = None
    extract: Optional[str] = Field(
        None,
        title="Metavariable whose content to use as the extracted result for subsequent rules",
    )
    dest_rules: Optional[DestRules] = Field(
        None,
        alias="dest-rules",
        title="Rules to include or exclude when running the destination language rules on the extracted file",
    )
    dest_language: Optional[str] = Field(
        None,
        alias="dest-language",
        title="Language to process the extracted result of this rule as",
    )
    transform: Optional[Transform] = Field(
        "no_transform", title="Method to transform the extracted content"
    )
    reduce: Optional[Reduce] = Field(
        "separate", title="Method of intrafile match reduction"
    )

    @field_validator("languages", mode="before")
    @classmethod
    def check_language(cls, v: Union[str, List[str]]) -> Union[str, List[str]]:
        return check_language_static(v)

    # NOTE: This is a workaround to avoid having to create N different flavors of the rule model
    #       to support the different types of `match` fields (one of which must be set).
    @model_validator(mode="after")
    def check_has_pattern(self) -> "Rule":
        mode = self.mode
        missing_pattern_error_message = "Expected one of `pattern`, `pattern-either`, `patterns`, `pattern-regex`, `match` to be present"
        """
        NOTE: To debug an individual rule, you can use the following code snippet:
        ```
        if (
            self.id.root
            == "go.grpc.security.grpc-server-insecure-connection.grpc-server-insecure-connection"
        ):
            breakpoint()
        ```
        This will allow you to inspect the rule object and see what's going on.
        Please note that omitting the `root` attribute will attempt to compare an instance of `Id` with a string,
        and the breakpoint will not be hit.
        """
        # NOTE: We use PydanticCustomError over ValueError to provide a more descriptive error message
        if mode == RuleMode.extract:  # EXPERIMENTAL
            if not all(
                [
                    self.id,
                    self.languages,
                    self.dest_language,
                    self.extract,
                ]
            ):
                raise PydanticCustomError(
                    "check_extract_has_required_fields",
                    "Expected `id`, `languages`, `dest-language`, `extract` to be present",
                )
            if not any(
                [
                    self.match,
                    self.pattern,
                    self.pattern_either,
                    self.pattern_regex,
                    self.patterns,
                ]
            ):
                raise PydanticCustomError(
                    "check_extract_has_pattern", missing_pattern_error_message
                )
        elif (
            mode == RuleMode.taint and not self.taint
        ):  # old-style taint rule without taint field
            if not all(
                [
                    self.id,
                    self.message is not None,
                    self.languages,
                    self.severity,
                    self.pattern_sinks,
                    self.pattern_sources,
                ]
            ):
                raise PydanticCustomError(
                    "check_taint_has_required_fields",
                    "Expected `id`, `message`, `languages`, `severity`, `pattern-sinks`, `pattern-sources` to be present",
                )
        elif (
            self.taint  # NOTE: Assume we don't want taint set to False to trigger this
        ):  # new-style taint rule with taint field set
            if not all(
                [
                    self.id,
                    self.message is not None,
                    self.languages,
                    self.severity,
                    self.taint,
                ]
            ):
                raise PydanticCustomError(
                    "check_taint_v2_has_required_fields",
                    "Expected `id`, `message`, `languages`, `severity`, `taint` to be present",
                )
        elif mode == RuleMode._join:  # EXPERIMENTAL
            if not all(
                [
                    self.id,
                    self.message is not None,
                    self.severity,
                    self.join,
                ]
            ):
                raise PydanticCustomError(
                    "check_join_has_required_fields",
                    "Expected `id`, `message`, `severity`, `join` to be present",
                )
        elif mode == RuleMode.search:
            if not all(
                [
                    self.id,
                    self.languages,
                    self.message is not None,
                    self.severity,
                ]
            ):
                raise PydanticCustomError(
                    "check_has_required_fields",
                    "Expected `id`, `languages`, `message`, `severity`, to be present",
                )
            if not any(
                [
                    self.match,
                    self.pattern,
                    self.pattern_either,
                    self.pattern_regex,
                    self.patterns,
                    # EXPERIMENTAL
                    self.r2c_internal_project_depends_on,
                    self.match,
                ]
            ):
                raise PydanticCustomError(
                    "check_has_pattern", missing_pattern_error_message
                )
        elif not any(
            [
                self.pattern,
                self.pattern_either,
                self.patterns,
                self.pattern_regex,
                # EXPERIMENTAL
                self.r2c_internal_project_depends_on,
                self.match,
            ]
        ):  # NOTE: This case should be unreachable
            raise PydanticCustomError(
                "check_fallback_has_pattern", missing_pattern_error_message
            )
        return self


# Our final exported model for the rule schema
class RuleModel(BaseModel):
    rules: List[Rule]
