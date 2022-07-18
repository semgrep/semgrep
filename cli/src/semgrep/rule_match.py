import binascii
import hashlib
import itertools
import textwrap
from collections import Counter
from datetime import datetime
from functools import total_ordering
from pathlib import Path
from typing import Any
from typing import Counter as CounterType
from typing import Dict
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple
from typing import TYPE_CHECKING
from uuid import UUID

from attrs import evolve
from attrs import field
from attrs import frozen

import semgrep.output_from_core as core
import semgrep.semgrep_interfaces.semgrep_output_v0 as out
from semgrep.constants import NOSEM_INLINE_COMMENT_RE
from semgrep.constants import RuleSeverity
from semgrep.external.pymmh3 import hash128  # type: ignore[attr-defined]
from semgrep.rule import Rule

if TYPE_CHECKING:
    from semgrep.rule import Rule


def rstrip(value: Optional[str]) -> Optional[str]:
    return value.rstrip() if value else None


@total_ordering
@frozen(eq=False)
class RuleMatch:
    """
    A section of code that matches a single rule (which is potentially many patterns).

    This is also often referred to as a finding.
    TODO: Rename this class to Finding?
    """

    match: core.CoreMatch

    # fields from the rule
    message: str = field(repr=False)
    severity: RuleSeverity
    metadata: Dict[str, Any] = field(repr=False, factory=dict)

    # Do not use this extra field! This prevents from having typed JSON output
    # TODO: instead of extra, we should use the more explicit fields:
    #  fixed_lines: Optional[Any] = field(default=None)
    #  dependency_match_only: Optional[bool] = field(default=None)
    #  dependency_matches: Optional[Any] = field(default=None)
    # but then this would require to remove the @frozen from this class
    # because autofix and dependency_aware and join_rule are actually monkey patching
    # this frozen class.
    # TODO: redundant with core.extra but we do some monkey patching on
    # this extra field which prevents to use directly core.extra (immutable)
    extra: Dict[str, Any] = field(repr=False, factory=dict)

    # fields derived from the rule
    # We call rstrip() for consistency with semgrep-core, which ignores whitespace
    # including newline chars at the end of multiline patterns
    fix: Optional[str] = field(converter=rstrip, default=None)
    fix_regex: Optional[out.FixRegex] = None

    # ???
    index: int = 0

    # Used only for indexing match based IDs since index uses syntactic IDs to
    # index meaning that there can be index collisions if we use it for mid
    match_based_index: int = 0

    # This is the accompanying formula from the rule that created the match
    # Used for pattern_based_id
    #
    # This could be derived, if we wanted to keep the rule as a field of the
    # match. Seems easier to just calculate it w/index
    match_formula_string: str = ""

    # None means we didn't check; ignore status is unknown
    is_ignored: Optional[bool] = field(default=None)

    # derived attributes
    lines: List[str] = field(init=False, repr=False)
    previous_line: str = field(init=False, repr=False)
    syntactic_context: str = field(init=False, repr=False)
    cli_unique_key: Tuple = field(init=False, repr=False)
    ci_unique_key: Tuple = field(init=False, repr=False)
    ordering_key: Tuple = field(init=False, repr=False)
    match_based_key: Tuple = field(init=False, repr=False)
    syntactic_id: str = field(init=False, repr=False)
    match_based_id: str = field(init=False, repr=False)

    # TODO: return a out.RuleId
    @property
    def rule_id(self) -> str:
        return self.match.rule_id.value

    @property
    def path(self) -> Path:
        return Path(self.match.location.path)

    @property
    def start(self) -> core.Position:
        return self.match.location.start

    @property
    def end(self) -> core.Position:
        return self.match.location.end

    @lines.default
    def get_lines(self) -> List[str]:
        """
        Return lines in file that this RuleMatch is referring to.

        Assumes file exists.

        Need to do on initialization instead of on read since file might not be the same
        at read time
        """
        # Start and end line are one-indexed, but the subsequent slice call is
        # inclusive for start and exclusive for end, so only subtract from start
        start_line = self.start.line - 1
        end_line = self.end.line

        if start_line == -1 and end_line == 0:
            # Completely empty file
            return []

        # buffering=1 turns on line-level reads
        with self.path.open(buffering=1, errors="replace") as fd:
            result = list(itertools.islice(fd, start_line, end_line))

        return result

    @previous_line.default
    def get_previous_line(self) -> str:
        """Return the line preceding the match, if any.

        This is meant for checking for the presence of a nosemgrep comment.
        This implementation was derived from the 'lines' method below.
        Refer to it for relevant comments.
        IT feels like a lot of duplication. Feel free to improve.
        """
        # see comments in '_get_lines' method
        start_line = self.start.line - 2
        end_line = start_line + 1
        is_empty_file = self.end.line <= 0

        if start_line < 0 or is_empty_file:
            # no previous line
            return ""

        with self.path.open(buffering=1, errors="replace") as fd:
            res = list(itertools.islice(fd, start_line, end_line))

        if res:
            return res[0]
        else:
            return ""

    @syntactic_context.default
    def get_syntactic_context(self) -> str:
        """
        The code that matched, with whitespace and nosem comments removed.

        This is useful to so that findings can be considered the same
        when `    5 == 5` is updated to `  5 == 5  # nosemgrep`,
        and thus CI systems don't retrigger notifications.
        """
        lines = [*self.lines]
        if len(lines) > 0:
            lines[0] = NOSEM_INLINE_COMMENT_RE.sub("", lines[0])
            lines[0] = lines[0].rstrip() + "\n"

        code = "".join(lines)  # the lines end with newlines already
        code = textwrap.dedent(code)
        code = code.strip()
        return code

    @cli_unique_key.default
    def get_cli_unique_key(self) -> Tuple:
        """
        A unique key designed with data-completeness & correctness in mind.

        Results in more unique findings than ci_unique_key.

        Used for deduplication in the CLI before writing output.
        """
        return (
            self.rule_id,
            str(self.path),
            self.start.offset,
            self.end.offset,
            self.message,
        )

    @ci_unique_key.default
    def get_ci_unique_key(self) -> Tuple:
        """
        A unique key designed with notification user experience in mind.

        Results in fewer unique findings than cli_unique_key.
        """
        try:
            path = self.path.relative_to(Path.cwd())
        except (ValueError, FileNotFoundError):
            path = self.path
        return (self.rule_id, str(path), self.syntactic_context, self.index)

    @ordering_key.default
    def get_ordering_key(self) -> Tuple:
        """
        Used to sort findings in output.

        Note that we often batch by rule ID when gathering matches,
        so the included self.rule_id will not do anything in those cases.

        The message field is included to ensure a consistent ordering
        when two findings match with different metavariables on the same code.
        """
        return (
            self.path,
            self.start,
            self.end,
            self.rule_id,
            self.message,
        )

    @syntactic_id.default
    def get_syntactic_id(self) -> str:
        """
        A 32-character hash representation of ci_unique_key.

        This value is sent to semgrep.dev and used as a unique key in the database.
        """
        # Upon reviewing an old decision,
        # there's no good reason for us to use MurmurHash3 here,
        # but we need to keep consistent hashes so we cannot change this easily
        hash_int = hash128(str(self.ci_unique_key))
        hash_bytes = int.to_bytes(hash_int, byteorder="big", length=16, signed=False)
        return str(binascii.hexlify(hash_bytes), "ascii")

    @match_based_key.default
    def get_match_based_key(self) -> Tuple:
        """
        A unique key with match based id's notion of uniqueness in mind.

        We use this to check if two different findings will have the same match
        based id or not. This is so we can then index them accordingly so two
        similar findings will have unique match based IDs
        """
        try:
            path = self.path.relative_to(Path.cwd())
        except (ValueError, FileNotFoundError):
            path = self.path
        match_formula_str = self.match_formula_string
        if self.extra.get("metavars") is not None:
            metavars = self.extra["metavars"]
            for metavar in metavars:
                match_formula_str = match_formula_str.replace(
                    metavar, metavars[metavar]["abstract_content"]
                )
        return (match_formula_str, path, self.rule_id)

    # This will supercede syntactic id, as currently that will change even if
    # things formatting + line numbers change. By using the formula +
    # metavariable content itself, we remain sensitive to modifications to a
    # match, but we no longer count formatting + line number changs + other
    # things as new findings
    @match_based_id.default
    def get_match_based_id(self) -> str:
        match_id = self.get_match_based_key()
        match_id_str = str(match_id)
        return f"{hashlib.blake2b(str.encode(match_id_str)).hexdigest()}_{str(self.match_based_index)}"

    @property
    def uuid(self) -> UUID:
        """
        A UUID representation of ci_unique_key.
        """
        return UUID(hex=self.syntactic_id)

    @property
    def is_blocking(self) -> bool:
        """
        Returns if this finding indicates it should block CI
        """
        return "block" in self.metadata.get("dev.semgrep.actions", ["block"])

    def to_app_finding_format(self, commit_date: str) -> out.Finding:
        """
        commit_date here for legacy reasons.
        commit date of the head commit in epoch time
        """
        commit_date_app_format = datetime.fromtimestamp(int(commit_date)).isoformat()

        # Follow semgrep.dev severity conventions
        if self.severity.value == RuleSeverity.ERROR.value:
            app_severity = 2
        elif self.severity.value == RuleSeverity.WARNING.value:
            app_severity = 1
        elif self.severity.value == RuleSeverity.EXPERIMENT.value:
            app_severity = 4
        else:
            app_severity = 0

        ret = out.Finding(
            check_id=out.RuleId(self.rule_id),
            path=str(self.path),
            line=self.start.line,
            column=self.start.col,
            end_line=self.end.line,
            end_column=self.end.col,
            message=self.message,
            severity=app_severity,
            index=self.index,
            commit_date=commit_date_app_format,
            syntactic_id=self.syntactic_id,
            match_based_id=self.match_based_id,
            metadata=out.RawJson(self.metadata),
            is_blocking=self.is_blocking,
            dataflow_trace=self.match.extra.dataflow_trace,
        )

        if self.extra.get("fixed_lines"):
            ret.fixed_lines = self.extra.get("fixed_lines")
        if "dependency_match_only" in self.extra and "dependency_matches" in self.extra:
            ret.sca_info = out.ScaInfo(
                dependency_match_only=self.extra["dependency_match_only"],
                dependency_matches=out.RawJson(self.extra["dependency_matches"]),
            )
        return ret

    def __hash__(self) -> int:
        """
        We use the "data-correctness" key to prevent keeping around duplicates.
        """
        return hash(self.cli_unique_key)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, type(self)):
            return False
        return self.cli_unique_key == other.cli_unique_key

    def __lt__(self, other: "RuleMatch") -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        return self.ordering_key < other.ordering_key


class RuleMatchSet(Iterable[RuleMatch]):
    """
    A custom set type which is aware when findings are the same.

    It also automagically adds the correct finding index when adding elements.
    """

    def __init__(
        self, rule: Rule, __iterable: Optional[Iterable[RuleMatch]] = None
    ) -> None:
        self._match_based_counts: CounterType[Tuple] = Counter()
        self._ci_key_counts: CounterType[Tuple] = Counter()
        self._rule = rule
        if __iterable is None:
            self._set = set()
        else:
            self._set = set(__iterable)

    def add(self, match: RuleMatch) -> None:
        """
        Add finding, but if the same (rule, path, code) exists,
        note this by incrementing the finding's index.

        The index lets us still notify when some code with findings is duplicated,
        even though we'd otherwise deduplicate the findings.
        """
        if match.rule_id != self._rule.id:
            raise ValueError("Added match must have identical rule id to set rule")
        match = evolve(match, match_formula_string=self._rule.formula_string)
        self._match_based_counts[match.get_match_based_key()] += 1
        self._ci_key_counts[match.ci_unique_key] += 1
        match = evolve(match, index=self._ci_key_counts[match.ci_unique_key] - 1)
        match = evolve(
            match,
            match_based_index=self._match_based_counts[match.get_match_based_key()] - 1,
        )
        self._set.add(match)

    def update(self, *rule_match_iterables: Iterable[RuleMatch]) -> None:
        """
        Add findings, but if the same (rule, path, code) exists,
        note this by incrementing the finding's index.

        The index lets us still notify when some code with findings is duplicated,
        even though we'd otherwise deduplicate the findings.
        """
        for rule_matches in rule_match_iterables:
            for rule_match in rule_matches:
                self.add(rule_match)

    def __iter__(self) -> Iterator[RuleMatch]:
        return iter(self._set)


# Our code orders findings at one point and then just assumes they're in order.
# This type marks variables that went through ordering already.
OrderedRuleMatchList = List[RuleMatch]
RuleMatchMap = Dict["Rule", OrderedRuleMatchList]
