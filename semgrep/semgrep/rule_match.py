import binascii
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
from typing import List
from typing import Optional
from typing import Set
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

    # None means we didn't check; ignore status is unknown
    is_ignored: Optional[bool] = field(default=None)

    # derived attributes
    lines: List[str] = field(init=False, repr=False)
    previous_line: str = field(init=False, repr=False)
    syntactic_context: str = field(init=False, repr=False)
    cli_unique_key: Tuple = field(init=False, repr=False)
    ci_unique_key: Tuple = field(init=False, repr=False)
    ordering_key: Tuple = field(init=False, repr=False)
    syntactic_id: str = field(init=False, repr=False)

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

        We use this to check if a finding matches its baseline equivalent,
        and to de-duplicate findings when pushing to semgrep.dev from CI,
        so that you don't get multiple notifications for the same finding
        when just moving it around commit-after-commit in a pull request.

        Some things that this key deduplicates to reduce useless notifications:
        - findings that match different metavariables within the same code snippet
        - findings that differ only in indentation
        - findings that differ only because one is ignored with `# nosemgrep`

        This key was originally implemented in and ported from semgrep-agent.
        """
        try:
            path = self.path.relative_to(Path.cwd())
        except ValueError:
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
            metadata=out.RawJson(out._Identity(self.metadata)),
            is_blocking=self.is_blocking,
        )

        if self.extra.get("fixed_lines"):
            ret.fixed_lines = self.extra.get("fixed_lines")
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


class RuleMatchSet(Set[RuleMatch]):
    """
    A custom set type which is aware when findings are the same.

    It also automagically adds the correct finding index when adding elements.
    """

    def __init__(self, __iterable: Optional[Iterable[RuleMatch]] = None) -> None:
        self._ci_key_counts: CounterType[Tuple] = Counter()
        if __iterable is None:
            super().__init__()
        else:
            super().__init__(__iterable)

    def add(self, match: RuleMatch) -> None:
        """
        Add finding, but if the same (rule, path, code) exists,
        note this by incrementing the finding's index.

        The index lets us still notify when some code with findings is duplicated,
        even though we'd otherwise deduplicate the findings.
        """
        self._ci_key_counts[match.ci_unique_key] += 1
        match = evolve(match, index=self._ci_key_counts[match.ci_unique_key] - 1)
        super().add(match)

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


# Our code orders findings at one point and then just assumes they're in order.
# This type marks variables that went through ordering already.
OrderedRuleMatchList = List[RuleMatch]
RuleMatchMap = Dict["Rule", OrderedRuleMatchList]
