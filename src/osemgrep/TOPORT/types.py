
Targets = FrozenSet[Path]

class FilteredFiles:
    """
    The return value of functions that filters target files.
    """

    kept: Targets
    removed: Targets = field(factory=frozenset)

class FilteredMatches:
    """
    The return value of functions that filter matches files.
    """

    kept: "RuleMatchMap"
    removed: "RuleMatchMap" = field(factory=lambda: defaultdict(list))
