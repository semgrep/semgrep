class SemgrepVariant:
    def __init__(self, name: str, semgrep_core_extra: str, semgrep_extra: str = ""):
        # name for the input corpus (rules and targets)
        self.name = name

        # space-separated extra arguments to pass to semgrep-core
        # command via SEMGREP_CORE_EXTRA environment variable
        self.semgrep_core_extra = semgrep_core_extra

        # space-separated extra arguments to pass to the default semgrep
        # command
        self.semgrep_extra = semgrep_extra


# Feel free to create new variants. The idea is to use the default set
# of options as the baseline and we see what happens when we enable or
# disable this or that optimization.
#
from constants import STD

SEMGREP_VARIANTS = [
    # default settings
    SemgrepVariant(STD, ""),
    # removing optimisations
    SemgrepVariant("no-cache", "-no_opt_cache"),
    SemgrepVariant("max-cache", "-opt_max_cache"),
    SemgrepVariant("no-bloom", "-no_bloom_filter"),
    SemgrepVariant("no-gc-tuning", "-no_gc_tuning"),
    # alternate optimisations
    SemgrepVariant("set_filters", "-set_filter"),
    SemgrepVariant("experimental", "-no_filter_irrelevant_rules", "--optimizations"),
    SemgrepVariant(
        "experimental_and_fast", "-filter_irrelevant_rules", "--optimizations"
    ),
]

# For when you just want to test a single change
STD_VARIANTS = [SemgrepVariant(STD, "")]

GITLAB_VARIANTS = [
    SemgrepVariant(STD, ""),
    SemgrepVariant("experimental", "-no_filter_irrelevant_rules", "--optimizations"),
]