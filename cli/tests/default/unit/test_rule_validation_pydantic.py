from os import listdir
from os import path
from os.path import isfile
from os.path import join
from unittest.mock import patch

import pytest
from pydantic import ValidationError
from ruamel.yaml import YAML

from semgrep.config_resolver import parse_config_string
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import validate_yaml_pydantic
from semgrep.rule_lang import YamlTree
from semgrep.rule_model import Languages
from semgrep.semgrep_types import LANGUAGE

yaml = YAML()

INVALID_RULES_PATH_PREFIX = join(
    path.dirname(__file__), "..", "e2e", "rules", "invalid-rules"
)
INVALID_RULES = [
    "invalid-pattern-child.yaml",
    "invalid-missing-top-item.yaml",
    "invalid-pattern-operator.yaml",
    "additional-invalid-pattern-operator.yaml",
    "string-pattern.yaml",
    "string-pattern-under-patterns.yaml",
    "missing-hyphen.yaml",
    "missing-pattern.yaml",
]


VALID_RULES_PATH_PREFIX = join(path.dirname(__file__), "..", "e2e", "rules")
# Currently only need to exclude one otherwise valid config as invalid rules due to
# min- and max-version constraints are filtered out in the E2E flow
VALID_RULE_EXCLUSIONS = {"version-constraints.yaml"}
VALID_RULES = [
    fname
    for fname in listdir(VALID_RULES_PATH_PREFIX)
    if isfile(join(VALID_RULES_PATH_PREFIX, fname))
    and not fname.startswith("bad")
    and not fname in VALID_RULE_EXCLUSIONS
]


@pytest.mark.quick
def test_supported_language_parity():
    """
    Ensure that our pydantic language model has parity with our language data (source of truth).
    """
    supported_languages = set(LANGUAGE.lang_by_key.keys())
    missed_languages = set()
    for lang in supported_languages:
        if not Languages.resolve(lang):
            missed_languages.add(lang)
    assert (
        not missed_languages
    ), f"Languages missing from pydantic Languages enum: {missed_languages}"


# This test is a little slow because it runs against 43 files
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule",
    [
        *VALID_RULES,
    ],
)
def test_validation_of_valid_rules_pydantic(rule):
    """
    Ensure that valid rules pass fast pydantic validation
    """
    content = ""
    with open(join(VALID_RULES_PATH_PREFIX, rule)) as fp:
        content = fp.read()
    data = YamlTree.wrap(yaml.load(content), EmptySpan)
    validate_yaml_pydantic(data)


# This test is a little slow because it runs against 43 files
@patch("jsonschema.validate")
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule",
    [
        *VALID_RULES,
    ],
)
def test_validation_of_valid_rules(jsonschema_mock, rule):
    """
    Ensure that valid rules pass fast pydantic validation and do not trigger jsonschema validation E2E
    """
    content = ""
    with open(join(VALID_RULES_PATH_PREFIX, rule)) as fp:
        content = fp.read()
    parse_config_string(rule, content, rule, True)
    assert not jsonschema_mock.called, "jsonschema.validate should not be called"


@pytest.mark.quick
@pytest.mark.parametrize(
    "rule",
    [
        *INVALID_RULES,
    ],
)
def test_validation_of_invalid_rules_pydantic(rule):
    """
    Ensure that valid rules fail fast pydantic validation
    """
    content = ""
    with open(join(INVALID_RULES_PATH_PREFIX, rule)) as fp:
        content = fp.read()
    data = YamlTree.wrap(yaml.load(content), EmptySpan)
    with pytest.raises(ValidationError):
        validate_yaml_pydantic(data)


@patch("jsonschema.validate")
@pytest.mark.quick
@pytest.mark.parametrize(
    "rule",
    [
        *INVALID_RULES,
    ],
)
def test_validation_of_invalid_rules(jsonschema_mock, rule):
    """
    Ensure that invalid rules fail fast pydantic validation and trigger jsonschema validation E2E
    """
    content = ""
    with open(join(INVALID_RULES_PATH_PREFIX, rule)) as fp:
        content = fp.read()
    parse_config_string(rule, content, rule, True)
    assert jsonschema_mock.called, "jsonschema.validate should be called"
