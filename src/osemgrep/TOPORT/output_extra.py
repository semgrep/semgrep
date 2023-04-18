
# This class exists to wrap some of the output returned by `semgrep-core`, on its way up
# through the call stack.
# This class is easily extendable if we want to add more information to the CLI output
# in the future.
@frozen
class OutputExtra:
    all_targets: Set[Path]
    profiling_data: ProfilingData
    parsing_data: ParsingData
    explanations: Optional[List[core.MatchingExplanation]]
    rules_by_engine: List[core.RuleIdAndEngineKind]
