"""
For each directory containing YAML rules, run those rules on the file in the same directory with the same name but different extension.
E.g. eqeq.yaml runs on eqeq.py.
Validate that the output is annotated in the source file with by looking for a comment like:

 ```
 # ruleid:eqeq-is-bad
 ```
 On the preceeding line.

 """
import collections
import functools
import json
import multiprocessing
import sys
import tarfile
from itertools import product
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple

from boltons.iterutils import partition

from semgrep.constants import BREAK_LINE
from semgrep.semgrep_main import invoke_semgrep
from semgrep.util import is_config_suffix
from semgrep.util import is_config_test_suffix
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

SAVE_TEST_OUTPUT_JSON = "semgrep_runs_output.json"
SAVE_TEST_OUTPUT_TAR = "semgrep_runs_output.tar.gz"

COMMENT_SYNTAXES = (("#", "\n"), ("//", "\n"), ("<!--", "-->"), ("(*", "*)"))
SPACE_OR_NO_SPACE = ("", " ")
TODORULEID = "todoruleid"
RULEID = "ruleid"
TODOOK = "todook"
OK = "ok"

EXIT_FAILURE = 2


def _remove_ending_comments(rule: str) -> str:
    for _, end in COMMENT_SYNTAXES:
        rule = rule.strip() if end == "\n" else rule.strip().replace(end, "")
    return rule


def normalize_rule_ids(line: str) -> Set[str]:
    """
    given a line like `     # ruleid:foobar`
    or `      // ruleid:foobar`
    return `foobar`
    """
    _, rules_text = line.strip().split(":")
    rules_text = rules_text.strip()
    rules = rules_text.split(",")
    # remove comment ends for non-newline comment syntaxes
    rules_clean = map(lambda rule: _remove_ending_comments(rule), rules)
    return set(filter(None, [rule.strip() for rule in rules_clean]))


def _annotations(annotation: str) -> Set[str]:
    # returns something like: {"#ruleid:", "# ruleid:", "//ruleid:", ...}
    return {
        f"{comment_syntax[0]}{space}{annotation}"
        for comment_syntax, space in product(COMMENT_SYNTAXES, SPACE_OR_NO_SPACE)
    }


def line_has_todo_rule(line: str) -> bool:
    todo_rule_annotations = _annotations(TODORULEID)
    return any(annotation in line for annotation in todo_rule_annotations)


def line_has_rule(line: str) -> bool:
    rule_annotations = _annotations(RULEID)
    return any(annotation in line for annotation in rule_annotations)


def line_has_ok(line: str) -> bool:
    rule_annotations = _annotations(OK)
    return any(annotation in line for annotation in rule_annotations)


def line_has_todo_ok(line: str) -> bool:
    rule_annotations = _annotations(TODOOK)
    return any(annotation in line for annotation in rule_annotations)


def _add_line_to_dict_of_ruleids(
    rule_ids: Set[str],
    line_dict: Dict[str, Dict[str, List[int]]],
    effective_line_num: int,
    test_file_resolved: str,
) -> None:
    """
    given that 'todoruleid' or 'todook' or 'ok' or 'todo' are detected, add the line number
    flagged to the appropriate dictionary.
    """
    for rule_id in rule_ids:
        line_dict[test_file_resolved][rule_id].append(effective_line_num)


def check_rule_id_mismatch(
    reported_lines: Dict[str, Dict[str, List[int]]], test_lines: Dict[str, Set]
) -> None:
    """
    checks whether there exists a #ruleid: <rule name> annotation where a rule matching <rule name> doesn't exist.
    leads to exit of 1 if there does exist such an occurence.
    """
    rule_id_mismatch = False
    if reported_lines:
        for file_path, test_ids in test_lines.items():
            reported_ids = set(reported_lines[file_path].keys())
            if test_ids.symmetric_difference(reported_ids):
                test_id_no_reported_ids = test_ids - reported_ids
                logger.error(
                    f"Found rule id mismatch - file={file_path} 'ruleid' annotation with no YAML rule={test_id_no_reported_ids}"
                )
                rule_id_mismatch = True

    if rule_id_mismatch:
        logger.error(
            "Failing due to rule id mismatch. There is a test denoted with 'ruleid: <rule name>' where the rule name does not exist or is not expected in the test file."
        )
        sys.exit(EXIT_FAILURE)


def get_expected_and_reported_lines(
    json_out: Dict[str, Any], test_files: List[Path]
) -> Dict[str, Dict[str, Any]]:
    """
    Collects the expected lines (which are the lines annotated with '#ruleid')
    and the reported lines (which are the lines that the run of semgrep flagged on)
    Returns the 'matches' dictionary, which maps check_ids -> file_paths involved -> expected
    and reported line lists.

    Note: we need matches to map check_ids -> file paths because some rule_ids have two
    distinct test files (notably, for rules that work on both tsx and jsx)
    """
    ruleid_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )
    ok_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )
    reported_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )
    todo_ok_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )
    todo_ruleid_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )

    matches_by_check_id: Dict[str, Dict[str, Any]] = collections.defaultdict(dict)

    for test_file in test_files:
        test_file_resolved = str(test_file.resolve())
        all_lines = test_file.read_text().split("\n")
        for i, line in enumerate(all_lines):
            # +1 because we are 0 based and semgrep output is not, plus skip the comment line
            effective_line_num = i + 2

            rule_in_line = line_has_rule(line)
            ok_in_line = line_has_ok(line)
            todo_rule_in_line = line_has_todo_rule(line)
            todo_ok_in_line = line_has_todo_ok(line)

            has_parseable_rule_id = (
                rule_in_line or todo_rule_in_line or ok_in_line or todo_ok_in_line
            ) and ":" in line

            try:
                if not has_parseable_rule_id:
                    continue
                rule_ids = normalize_rule_ids(line)
                if todo_rule_in_line or rule_in_line:
                    _add_line_to_dict_of_ruleids(
                        rule_ids, ruleid_lines, effective_line_num, test_file_resolved
                    )
                if todo_rule_in_line or ok_in_line:
                    _add_line_to_dict_of_ruleids(
                        rule_ids, ok_lines, effective_line_num, test_file_resolved
                    )
                if todo_ok_in_line:
                    _add_line_to_dict_of_ruleids(
                        rule_ids, todo_ok_lines, effective_line_num, test_file_resolved
                    )
                if todo_rule_in_line:
                    _add_line_to_dict_of_ruleids(
                        rule_ids,
                        todo_ruleid_lines,
                        effective_line_num,
                        test_file_resolved,
                    )
            except ValueError:  # comment looked like a test annotation but couldn't parse
                logger.warning(
                    f"Could not parse {line} as a test annotation in file {test_file_resolved}. Skipping this line"
                )
                continue

    for result in json_out["results"]:
        path = str(Path(result["path"]).resolve())
        check_id = result["check_id"]
        start_line = int(result["start"]["line"])
        reported_lines[path][check_id].append(start_line)

    test_lines: Dict[str, Set] = collections.defaultdict(set)
    for lines in [ruleid_lines, ok_lines]:
        for file_path, test_annotations in lines.items():
            test_lines[file_path].update(test_annotations.keys())

    check_rule_id_mismatch(reported_lines, test_lines)

    def join_keys(a: Dict[str, Any], b: Dict[str, Any]) -> Set[str]:
        return set(a.keys()).union(set(b.keys()))

    for file_path in join_keys(ruleid_lines, reported_lines):
        for check_id in join_keys(ruleid_lines[file_path], reported_lines[file_path]):
            all_reported = set(reported_lines[file_path][check_id])
            expected = set(ruleid_lines[file_path][check_id])
            todo_oked = set(todo_ok_lines[file_path][check_id])
            todo_ruleid = set(todo_ruleid_lines[file_path][check_id])

            reported = all_reported - todo_oked - todo_ruleid
            expected = expected - todo_ruleid - todo_oked

            matches_by_check_id[check_id][file_path] = {
                "expected_lines": sorted(expected),
                "reported_lines": sorted(reported),
            }

    return matches_by_check_id


def _generate_check_output_line(check_id: str, check_results: Mapping[str, Any]) -> str:
    def _generate_expected_vs_reported_lines(matches: Mapping[str, Any]) -> str:
        expected = f"expected lines: {matches['expected_lines']}"
        reported = f"reported lines: {matches['reported_lines']}"
        return f"{expected}, {reported}"

    expected_vs_reported_lines = "\t\n".join(
        _generate_expected_vs_reported_lines(matches)
        for _, matches in check_results["matches"].items()
    )

    test_file_names = " ".join(
        test_file for test_file, _ in check_results["matches"].items()
    )

    return f"\t✖ {check_id.ljust(60)} {expected_vs_reported_lines} \n\t test file path: {test_file_names}\n\n"


def invoke_semgrep_multi(
    config: Path, targets: List[Path], **kwargs: Any
) -> Tuple[Path, Optional[str], Any]:
    try:
        output = invoke_semgrep(config, targets, **kwargs)
    except Exception as error:
        # We must get the string of the error because the multiprocessing library
        # will fail the marshal the error and hang
        # See: https://bugs.python.org/issue39751
        return (config, str(error), {})
    else:
        return (config, None, output)


def relatively_eq(parent1: Path, child1: Path, parent2: Path, child2: Path) -> bool:
    def remove_all_suffixes(p: Path) -> Path:
        result = p.with_suffix("")
        while result != result.with_suffix(""):
            result = result.with_suffix("")
        return result

    rel1 = remove_all_suffixes(child1.relative_to(parent1))
    rel2 = remove_all_suffixes(child2.relative_to(parent2))
    return rel1 == rel2


def get_config_filenames(original_config: Path) -> List[Path]:
    if original_config.is_file():
        return [original_config]
    configs = list(original_config.rglob("*"))
    return [
        config
        for config in configs
        if is_config_suffix(config)
        and not config.name.startswith(".")
        and not config.parent.name.startswith(".")
    ]


def get_config_test_filenames(
    original_config: Path, configs: List[Path], original_target: Path
) -> Dict[Path, List[Path]]:
    original_config_is_file_not_directory = original_config.is_file()
    original_target_is_file_not_directory = original_target.is_file()

    if original_config_is_file_not_directory and original_target_is_file_not_directory:
        return {original_config: [original_target]}

    if original_target_is_file_not_directory:
        targets = list(original_target.parent.rglob("*"))
    else:
        targets = list(original_target.rglob("*"))

    def target_matches_config(target: Path, config: Path) -> bool:
        correct_suffix = is_config_test_suffix(target) or not is_config_suffix(target)
        return (
            (
                original_target_is_file_not_directory
                or relatively_eq(original_target, target, original_config, config)
            )
            and target.is_file()
            and correct_suffix
        )

    return {
        config: [target for target in targets if target_matches_config(target, config)]
        for config in configs
    }


def checkid_passed(matches_for_checkid: Dict[str, Any]) -> bool:
    for _filename, expected_and_reported_lines in matches_for_checkid.items():
        if (
            not expected_and_reported_lines["expected_lines"]
            == expected_and_reported_lines["reported_lines"]
        ):
            return False
    return True


def generate_test_results(
    *,
    target: Path,
    config: Path,
    strict: bool,
    json_output: bool,
    deep: bool,
    save_test_output_tar: bool = True,
    optimizations: str = "none",
) -> None:
    config_filenames = get_config_filenames(config)
    config_test_filenames = get_config_test_filenames(config, config_filenames, target)
    config_with_tests, config_without_tests = partition(
        config_test_filenames.items(),
        lambda c: bool(c[1]),
    )
    config_missing_tests_output = [str(c[0]) for c in config_without_tests]

    invoke_semgrep_fn = functools.partial(
        invoke_semgrep_multi,
        deep=deep,
        no_git_ignore=True,
        no_rewrite_rule_ids=True,
        strict=strict,
        optimizations=optimizations,
    )
    with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
        results = pool.starmap(invoke_semgrep_fn, config_with_tests)

    config_with_errors, config_without_errors = partition(results, lambda r: bool(r[1]))
    config_with_errors_output = [
        {"filename": str(filename), "error": error, "output": output}
        for filename, error, output in config_with_errors
    ]

    tested = {
        filename: get_expected_and_reported_lines(
            output, config_test_filenames[filename]
        )
        for filename, _, output in config_without_errors
    }

    results_output: Mapping[str, Mapping[str, Any]] = {
        str(filename): {
            "checks": {
                check_id: {
                    "passed": checkid_passed(filename_and_matches),
                    "matches": filename_and_matches,
                }
                for check_id, filename_and_matches in matches.items()
            }
        }
        for filename, matches in tested.items()
    }

    output = {
        "config_missing_tests": config_missing_tests_output,
        "config_with_errors": config_with_errors_output,
        "results": results_output,
    }

    strict_error = bool(config_with_errors_output) and strict
    any_failures = any(
        not check_results["passed"]
        for file_results in results_output.values()
        for check_results in file_results["checks"].values()
    )
    exit_code = int(strict_error or any_failures)

    if json_output:
        print(json.dumps(output, indent=4, separators=(",", ": ")))
        sys.exit(exit_code)

    # save the results to json file and tar the file to upload as github artifact.
    if save_test_output_tar:
        list_to_output = []
        with open(SAVE_TEST_OUTPUT_JSON, "w") as f:
            for tup in results:
                true_result = tup[2]
                list_to_output.append(true_result)
            f.write(json.dumps(list_to_output, indent=4, separators=(",", ":")))

        with tarfile.open(SAVE_TEST_OUTPUT_TAR, "w:gz") as tar:
            tar.add(SAVE_TEST_OUTPUT_JSON)

    num_tests = 0
    num_tests_passed = 0
    check_output_lines: str = ""
    for _filename, rr in results_output.items():
        for check_id, check_results in sorted(rr["checks"].items()):
            num_tests += 1
            if not check_results["passed"]:
                check_output_lines += _generate_check_output_line(
                    check_id, check_results
                )
            else:
                num_tests_passed += 1

    if num_tests == 0:
        print(
            "No unit tests found. See https://semgrep.dev/docs/writing-rules/testing-rules"
        )
    elif num_tests == num_tests_passed:
        print(f"{num_tests_passed}/{num_tests}: ✓ All tests passed ")
    else:
        print(
            f"{num_tests_passed}/{num_tests}: {num_tests - num_tests_passed} unit tests did not pass:"
        )
        print(BREAK_LINE)
        print(check_output_lines)

    if config_with_errors_output:
        print(BREAK_LINE)
        print("The following config files produced errors:")
        print(
            "\t"
            + "\n\t".join(
                f"{c['filename']}: {c['error']}" for c in config_with_errors_output
            )
        )

    sys.exit(exit_code)


def test_main(
    *,
    target: Sequence[str],
    config: Optional[Sequence[str]],
    test_ignore_todo: bool,
    strict: bool,
    json: bool,
    save_test_output_tar: bool,
    optimizations: str,
    deep: bool,
) -> None:

    if len(target) != 1:
        raise Exception("only one target directory allowed for tests")
    target_path = Path(target[0])

    if config:
        if len(config) != 1:
            raise Exception("only one config directory allowed for tests")
        config_path = Path(config[0])
    else:
        if target_path.is_file():
            raise Exception("--config is required when running a test on single file")
        config_path = target_path

    generate_test_results(
        target=target_path,
        config=config_path,
        strict=strict,
        json_output=json,
        deep=deep,
        save_test_output_tar=save_test_output_tar,
        optimizations=optimizations,
    )
