from pathlib import Path
from semgrep.rule import Rule
from semgrep.target_manager import TargetManager
from semgrep.profile_manager import ProfileManager
from semgrep.rule_match import RuleMatch
import semgrep.error as error
import semgrep.profiling as profiling
import semgrep.semgrep_types as semgrep_types
from typing import Any, Dict, List, Sequence, Set, Tuple

class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """
    def __init__(self,
                 jobs: int,
                 timeout: int,
                 max_memory: int,
                 timeout_threshold: int,
                 optimizations: str) -> None: ...

    def invoke_semgrep(self,
                       target_manager: TargetManager,
                       profiler: ProfileManager,
                       rules: List[Rule]) -> Tuple[Dict[Rule, List[RuleMatch]],
                                                   List[error.SemgrepError],
                                                   Set[Path],
                                                   profiling.ProfilingData]:
        """
        Takes in rules and targets and retuns object with findings
        """
        ...

    def validate_configs(self,
                         configs: Tuple[str, ...]) -> Sequence[error.SemgrepError]:
        ...
