from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import Mapping
from pathlib import Path
from semgrep.rule import Rule
import semgrep.error as error
import semgrep.types as types
import semgrep.rule_lang as rule_lang

class ConfigPath:
    def __init__(self, config_str: str, project_url: Optional[str]) -> None:
        """
        Mutates MetricManager state!
        Takes a user's inputted config_str and transforms it into the appropriate
        path, checking whether the config string is a registry url or not. If it
        is, also set the appropriate MetricManager flag
        """
        ...

class Config:
    def __init__(self, valid_configs: Mapping[str, Sequence[Rule]]) -> None:
        """
        Handles parsing and validating of config files
        and exposes ability to get all rules in parsed config files
        """
        ...

    @classmethod
    def from_config_list(
        cls, configs: Sequence[str], project_url: Optional[str]
    ) -> Tuple["Config", Sequence[error.SemgrepError]]:
        """
        Takes in list of files/directories and returns Config object as well as
        list of errors parsing said config files

        If empty list is passed, tries to read config file at default locations
        """
        ...

    #TODO: prefixed with _ but actuall used in join_rule.py
    @staticmethod
    def _validate(
        config_dict: Mapping[str, rule_lang.YamlTree]
    ) -> Tuple[Mapping[str, Sequence[Rule]], Sequence[error.SemgrepError]]:
        """
        Take configs and separate into valid and list of errors parsing the invalid ones
        """



def resolve_targets(targets: Sequence[str]) -> Sequence[Path]:
    ...

def get_base_path() -> Path:
    ...

def get_config(
    pattern: Optional[str],
    lang: Optional[str],
    config_strs: Sequence[str],
    *,
    project_url: Optional[str],
    replacement: Optional[str] = None,
) -> Tuple[Config, Sequence[error.SemgrepError]]:
    ...

def list_current_public_rulesets() -> List[types.JsonObject]:
    ...
