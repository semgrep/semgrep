##############################################################################
# Prelude
##############################################################################
# Generate what will be passed to semgrep-core via --targets
# and specified now in semgrep_output_v1.atd
import collections
from functools import lru_cache
from typing import DefaultDict
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Set
from typing import Tuple
from typing import Union

from attr import define
from attr import field
from attr import frozen
from boltons.iterutils import get_path
from rich import box
from rich.style import Style
from rich.table import Table

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.rule import Rule
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_types import Language
from semgrep.state import get_state
from semgrep.subproject import count_resolved_dependencies
from semgrep.subproject import get_display_paths
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


##############################################################################
# Helpers
##############################################################################
@frozen
class Task:
    path: str = field(converter=str)
    analyzer: Language  # Analyzer; see Analyzer.mli
    products: Tuple[out.Product, ...]
    # semgrep-core no longer uses the rule_nums field.
    # We're keeping it for now because it's needed by
    # 'split_by_lang_label_for_product'.
    # a rule_num is the rule's index in the rule ID list
    rule_nums: Tuple[int, ...]

    @property
    def language_label(self) -> str:
        return (
            "<multilang>"
            if not self.analyzer.definition.is_target_language
            else self.analyzer.definition.id
        )

    def to_target(self) -> out.Target:
        # Once we start sending supply chain rules to semgrep-core,
        # we'll need to start sending LockfileTargets as well
        return out.Target(
            out.CodeTarget_(
                out.CodeTarget(
                    path=out.Fpath(self.path),
                    analyzer=out.Analyzer(self.analyzer),
                    products=list(self.products),
                )
            )
        )


class TargetMappings(List[Task]):
    @property
    def rule_count(self) -> int:
        return len({rule_num for task in self for rule_num in task.rule_nums})

    @property
    def file_count(self) -> int:
        # the '<multilang>' label is reserved for regex & generic
        # (& others like secrets), which causes a double count
        # in the number of files.
        if not self[0].language_label == "<multilang>":
            return len(self)
        else:
            lang_count = len({task.analyzer.definition.id for task in self})
            return len(self) // lang_count


@define
class TaskCounts:
    files: int = 0
    rules: int = 0


##############################################################################
# Entry point
##############################################################################
class Plan:
    """
    Saves and displays knowledge of what will be run

    to_json: creates the json passed to semgrep_core -
    log: outputs a summary of how many files will be scanned for each file
    """

    def __init__(
        self,
        mappings: List[Task],
        rules: List[Rule],
        *,
        product: Optional[out.Product] = None,
        all_subprojects: Optional[
            List[Union[out.ResolvedSubproject, out.UnresolvedSubproject]]
        ],
        unused_rules: Optional[List[Rule]] = None,
    ):
        self.target_mappings = TargetMappings(mappings)
        # important: this is a list of rule_ids, not a set
        # target_mappings relies on the index of each rule_id in rule_ids
        self.rules = rules
        self.product = product
        self.all_subprojects = all_subprojects
        self.unused_rules = unused_rules or []

    # TODO: make this counts_by_lang_label, returning TaskCounts
    def split_by_lang_label(self) -> Dict[str, "TargetMappings"]:
        return self.split_by_lang_label_for_product()

    # Divides rule mapping up into the rule counts per language
    # filtering out rules for a specific product. If product = None
    # then all products are included.
    def split_by_lang_label_for_product(
        self, product: Optional[out.Product] = None
    ) -> Dict[str, "TargetMappings"]:
        result: Dict[str, TargetMappings] = collections.defaultdict(TargetMappings)
        for task in self.target_mappings:
            result[task.language_label].append(
                task
                if product is None
                else Task(
                    path=task.path,
                    analyzer=task.analyzer,
                    products=(product,),
                    rule_nums=tuple(
                        num
                        for num in task.rule_nums
                        if self.rules[num].product == product
                    ),
                )
            )
        return result

    @lru_cache(maxsize=1000)  # caching this saves 60+ seconds on mid-sized repos
    def ecosystems_by_rule_nums(self, rule_nums: Tuple[int]) -> Set[Ecosystem]:
        return {
            ecosystem
            for rule_num in rule_nums
            for ecosystem in self.rules[rule_num].ecosystems
        }

    def counts_by_ecosystem(
        self,
    ) -> Mapping[Ecosystem, TaskCounts]:
        result: DefaultDict[Ecosystem, TaskCounts] = collections.defaultdict(TaskCounts)

        # if a pypi rule does reachability analysis on *.json files,
        # when the user has no .json files, then there is no task for it,
        # but we should still print it as a reachability rule we used
        # so we get rule counts by looking at all rules
        for rule in self.rules:
            for ecosystem in rule.ecosystems:
                result[ecosystem].rules += 1

        # one .json file could determine the reachability of libraries from pypi and npm at the same time
        # so one task might need increase counts for multiple ecosystems (unlike when splitting by lang)
        for task in self.target_mappings:
            for ecosystem in self.ecosystems_by_rule_nums(task.rule_nums):
                result[ecosystem].files += 1

        # if a rule scans npm and maven, but we only have npm lockfiles,
        # then we skip mentioning maven in debug info by deleting maven's counts
        if self.all_subprojects is not None:
            used_ecosystems: Set[Union[Ecosystem, None]] = {
                s.info.ecosystem for s in self.all_subprojects
            }
            unused_ecosystems = {
                ecosystem for ecosystem in result if ecosystem not in used_ecosystems
            }
            for ecosystem in unused_ecosystems:
                del result[ecosystem]

        return result

    def to_targets(self) -> out.Targets:
        """Produce the input to semgrep-core in the form of a list of target files"""
        return out.Targets(
            out.Targets_([task.to_target() for task in self.target_mappings])
        )

    @property
    def num_targets(self) -> int:
        return len(self.target_mappings)

    def rule_count_for_product(self, product: out.Product) -> int:
        rule_nums: Set[int] = set()
        for task in self.target_mappings:
            for rule_num in task.rule_nums:
                if self.rules[rule_num].product == product:
                    rule_nums.add(rule_num)
        return len(rule_nums)

    def table_by_language(
        self, with_tables_for: Optional[out.Product] = None, use_color: bool = True
    ) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Language", header_style=Style(color=None, bold=use_color))
        table.add_column(
            "Rules", justify="right", header_style=Style(color=None, bold=use_color)
        )
        table.add_column(
            "Files", justify="right", header_style=Style(color=None, bold=use_color)
        )

        plans_by_language = sorted(
            self.split_by_lang_label_for_product(with_tables_for).items(),
            key=lambda x: (x[1].file_count, x[1].rule_count),
            reverse=True,
        )
        for language, plan in plans_by_language:
            if plan.rule_count:
                table.add_row(language, str(plan.rule_count), str(plan.file_count))

        return table

    def table_by_subproject(self) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Dependency Sources", overflow="fold")
        table.add_column("Resolution Method")
        table.add_column("Ecosystem")
        table.add_column("Dependencies")
        table.add_column("Rules", justify="right")

        counts_by_ecosystem = self.counts_by_ecosystem()

        RESOLUTION_METHOD_STRINGS = {
            out.LockfileParsing(): "Lockfile",
            out.DynamicResolution(): "Local build",
        }

        UNRESOLVED_REASON_STRINGS = {
            out.UnresolvedFailed(): "Failed",
            out.UnresolvedSkipped(): "Skipped",
            out.UnresolvedUnsupported(): "Unsupported",
            out.UnresolvedDisabled(): "Disabled (use --allow-local-builds)",
        }

        if self.all_subprojects is not None:
            for subproject in self.all_subprojects:
                if isinstance(subproject, out.ResolvedSubproject):
                    table.add_row(
                        ", ".join(
                            [
                                str(p)
                                for p in get_display_paths(
                                    subproject.info.dependency_source
                                )
                            ]
                        ),
                        RESOLUTION_METHOD_STRINGS[subproject.resolution_method.value],
                        subproject.ecosystem.kind,
                        str(
                            count_resolved_dependencies(
                                subproject.resolved_dependencies
                            )
                        ),
                        str(counts_by_ecosystem[subproject.ecosystem].rules),
                    )

                else:
                    table.add_row(
                        ", ".join(
                            [
                                str(p)
                                for p in get_display_paths(
                                    subproject.info.dependency_source
                                )
                            ]
                        ),
                        UNRESOLVED_REASON_STRINGS[subproject.reason.value],
                        "Unknown",
                        "-",
                        "-",
                    )

        return table

    def table_by_origin(
        self, with_tables_for: Optional[out.Product] = None, use_color: bool = True
    ) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Origin", header_style=Style(color=None, bold=use_color))
        table.add_column(
            "Rules", justify="right", header_style=Style(color=None, bold=use_color)
        )

        origin_counts = collections.Counter(
            get_path(rule.metadata, ("semgrep.dev", "rule", "origin"), default="custom")
            for rule in self.rules
            if rule.product == with_tables_for
        )

        for origin, count in sorted(
            origin_counts.items(), key=lambda x: x[1], reverse=True
        ):
            origin_name = origin.replace("_", " ").capitalize()

            table.add_row(origin_name, str(count))

        return table

    def table_by_sca_analysis(self) -> Table:
        table = Table(box=box.SIMPLE_HEAD, show_edge=False)
        table.add_column("Analysis")
        table.add_column("Rules", justify="right")

        SCA_ANALYSIS_NAMES = {
            "reachable": "Reachability",
            "legacy": "Basic",
            "malicious": "Basic",
            "upgrade-only": "Basic",
        }

        sca_analysis_counts = collections.Counter(
            SCA_ANALYSIS_NAMES.get(rule.metadata.get("sca-kind", ""), "Unknown")
            for rule in self.rules
            if isinstance(rule.product.value, out.SCA)
        )

        for sca_analysis, count in sorted(
            sca_analysis_counts.items(), key=lambda x: x[1], reverse=True
        ):
            sca_analysis_name = sca_analysis.replace("_", " ").title()

            table.add_row(sca_analysis_name, str(count))

        return table

    def record_metrics(self) -> None:
        metrics = get_state().metrics

        for language in self.split_by_lang_label():
            metrics.add_feature("language", language)

    def __str__(self) -> str:
        return f"<Plan of {len(self.target_mappings)} tasks for {list(self.split_by_lang_label())}>"
