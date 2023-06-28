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
import difflib
import functools
import json
import multiprocessing
import os
import shutil
import sys
import tempfile
import uuid
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
from ruamel.yaml import YAML

from semgrep.constants import BREAK_LINE
from semgrep.engine import EngineType
from semgrep.semgrep_main import invoke_semgrep
from semgrep.util import final_suffix_matches
from semgrep.util import is_config_fixtest_suffix
from semgrep.util import is_config_suffix
from semgrep.util import is_config_test_suffix
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


COMMENT_SYNTAXES = (("#", "\n"), ("//", "\n"), ("<!--", "-->"), ("(*", "*)"))
SPACE_OR_NO_SPACE = ("", " ")
TODORULEID = "todoruleid"
RULEID = "ruleid"
TODOOK = "todook"
OK = "ok"
DEEPOK = "deepok"
DEEPRULEID = "deepruleid"

EXIT_FAILURE = 2


def _remove_ending_comments(rule: str) -> str:
    for _, end in COMMENT_SYNTAXES:
        rule = rule.strip() if end == "\n" else rule.strip().replace(end, "")
    return rule


def normalize_rule_ids(line: str) -> Set[str]:
    """
    given a line like `     # ruleid:foobar`
    or `      // ruleid:foobar`
    or `      // ruleid:deepok:foobar`
    return `foobar`
    """
    _, rules_text = line.strip().split(":", 1)
    rules_text = rules_text.strip()
    # strip out "deepok" and "deepruleid" annotations if they are there to get rule name
    if rules_text.startswith(DEEPOK) or rules_text.startswith(DEEPRULEID):
        _, rules_text = rules_text.split(":")
    rules = rules_text.strip().split(",")
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
            except (
                ValueError
            ):  # comment looked like a test annotation but couldn't parse
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
    soft_errors = check_results["errors"]
    json_error_report = []
    if soft_errors:
        # Display partial parsing errors and such. These are tolerated
        # when running a semgrep scan but fatal in test mode.
        json_error_report = [f"errors: {json.dumps(soft_errors, indent=2)}"]

    def _generate_missed_vs_incorrect_lines(matches: Mapping[str, Any]) -> str:
        expected_lines = matches["expected_lines"]
        reported_lines = matches["reported_lines"]
        missed = f"missed lines: {list(set(expected_lines) - set(reported_lines))}"
        incorrect = (
            f"incorrect lines: {list(set(reported_lines) - set(expected_lines))}"
        )
        return f"{missed}, {incorrect}"

    missed_vs_incorrect_lines = [
        _generate_missed_vs_incorrect_lines(matches)
        for _, matches in check_results["matches"].items()
    ]
    all_errors = "\n\t".join(json_error_report + missed_vs_incorrect_lines)

    test_file_names = " ".join(
        test_file for test_file, _ in check_results["matches"].items()
    )

    return f"\t✖ {check_id}\n\t{all_errors}\n\ttest file path: {test_file_names}\n\n"


def _generate_fixcheck_output_line(
    filename: Path, diff: List[str], fixtest: Path
) -> str:
    diff_lines = "\n\t".join(diff)
    return f"\t✖ {fixtest} <> autofix applied to {filename} \n\n\t{diff_lines}\n\n\n"


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


def create_temporary_copy(path: Path) -> str:
    temp_dir = tempfile.gettempdir()
    _filename, file_extension = os.path.splitext(path)
    temp_path = os.path.join(temp_dir, str(uuid.uuid4()) + file_extension)
    shutil.copy2(path, temp_path)
    return temp_path


def relatively_eq(
    parent_target: Path, target: Path, parent_config: Path, config: Path
) -> bool:
    def remove_all_suffixes(p: str) -> str:
        return p.split(".", 1)[0]

    rel1 = target.relative_to(parent_target).parts
    rel2 = config.relative_to(parent_config).parts
    s = len(rel2)
    if len(rel1) < s:
        return False
    s -= 1
    return rel1[:s] == rel2[:s] and remove_all_suffixes(rel1[s]) == remove_all_suffixes(
        rel2[s]
    )


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
        correct_suffix = (
            is_config_test_suffix(target) or not is_config_suffix(target)
        ) and not is_config_fixtest_suffix(target)

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


def get_config_fixtest_filenames(
    original_target: Path, targets: Dict[Path, List[Path]]
) -> Dict[Path, List[Tuple[Path, Path]]]:
    original_target_is_file_not_directory = original_target.is_file()

    if original_target_is_file_not_directory:
        fixtests = list(original_target.parent.rglob("*"))
    else:
        fixtests = list(original_target.rglob("*"))

    def fixtest_matches_target(target: Path, fixtest: Path) -> bool:
        correct_suffix = is_config_fixtest_suffix(fixtest) and final_suffix_matches(
            target, fixtest
        )

        if original_target_is_file_not_directory:
            original_target_directory = original_target.parent
        else:
            original_target_directory = original_target

        return correct_suffix and relatively_eq(
            original_target_directory, target, original_target_directory, fixtest
        )

    return {
        config: [
            (target, fixtest)
            for target in targets[config]
            for fixtest in fixtests
            if fixtest_matches_target(target, fixtest)
        ]
        for config in targets
    }


def config_contains_fix_key(config: Path) -> bool:
    with open(config) as file:
        yaml = YAML(typ="safe")  # default, if not specfied, is 'rt' (round-trip)
        rule = yaml.load(file)
        if rule.get("rules"):
            return "fix" in rule["rules"][0]
        else:
            return False


def checkid_passed(matches_for_checkid: Dict[str, Any]) -> bool:
    for _filename, expected_and_reported_lines in matches_for_checkid.items():
        if (
            not expected_and_reported_lines["expected_lines"]
            == expected_and_reported_lines["reported_lines"]
        ):
            return False
    return True


def fixed_file_comparison(
    fixed_testfile: Path, expected_fixed_testfile: str
) -> List[str]:
    diff = []
    with open(fixed_testfile) as file1, open(expected_fixed_testfile) as file2:
        lines1 = file1.readlines()
        lines2 = file2.readlines()

        for line in difflib.unified_diff(lines1, lines2, n=0):
            diff.append(line.strip())

    return diff


def generate_test_results(
    *,
    target: Path,
    config: Path,
    strict: bool,
    json_output: bool,
    engine_type: EngineType,
    optimizations: str = "none",
) -> None:
    config_filenames = get_config_filenames(config)
    config_test_filenames = get_config_test_filenames(config, config_filenames, target)

    # Extract the subset of tests using autofix.
    # They'll be run later again, with autofix on (why run them twice?).
    config_fixtest_filenames: Dict[
        Path, List[Tuple[Path, Path]]
    ] = get_config_fixtest_filenames(target, config_test_filenames)

    config_with_tests, config_without_tests = partition(
        config_test_filenames.items(),
        lambda c: bool(c[1]),
    )

    config_missing_tests_output = [str(c[0]) for c in config_without_tests]

    invoke_semgrep_fn = functools.partial(
        invoke_semgrep_multi,
        engine_type=engine_type,
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

    tested = [
        (
            filename,
            get_expected_and_reported_lines(output, config_test_filenames[filename]),
            output["errors"] if "errors" in output else [],
        )
        for filename, _, output in config_without_errors
    ]

    results_output: Mapping[str, Mapping[str, Any]] = {
        str(filename): {
            "checks": {
                check_id: {
                    "passed": checkid_passed(filename_and_matches) and not errors,
                    "matches": filename_and_matches,
                    "errors": errors,
                }
                for check_id, filename_and_matches in matches.items()
            }
        }
        for filename, matches, errors in tested
    }

    fixtest_filenames: Dict[Path, List[Tuple[Path, Path]]] = {
        config: [
            (target, fixtest)
            for target, fixtest in testfiles
            # if os.path.abspath(target) in passed_test_filenames
        ]
        for config, testfiles in config_fixtest_filenames.items()
    }

    # List[Tuple[Path, List[Tuple[Path,Path]]]]
    config_with_fixtests, config_without_fixtests = partition(
        fixtest_filenames.items(), lambda c: bool(c[1])
    )

    configs_missing_fixtests = [
        str(c) for c, _fixtest in config_without_fixtests if config_contains_fix_key(c)
    ]

    # this saves execution time: fix will not be correct, if regular test is not correct
    passed_test_filenames = [
        filename
        for _config_filename, matches, soft_errors in tested
        for _check_id, filename_and_matches in matches.items()
        for filename, expected_and_reported_lines in filename_and_matches.items()
        if expected_and_reported_lines["expected_lines"]
        == expected_and_reported_lines["reported_lines"]
        and not soft_errors
    ]
    configs_with_fixtests = {
        config: [
            (target, fixtest)
            for target, fixtest in testfiles
            if os.path.abspath(target) in passed_test_filenames
        ]
        for config, testfiles in config_with_fixtests
    }

    temp_copies: Dict[Path, str] = {
        target: create_temporary_copy(target)
        for _config, testfiles in config_with_fixtests
        for target, _fixtest in testfiles
    }

    config_with_tempfiles = [
        (config, [temp_copies[target] for target, _fixtest in testfiles])
        for config, testfiles in config_with_fixtests
    ]

    # This is the invocation of semgrep for testing autofix.
    #
    # TODO: should 'engine' be set to 'engine=engine' or always 'engine=EngineType.OSS'?
    invoke_semgrep_with_autofix_fn = functools.partial(
        invoke_semgrep_multi,
        no_git_ignore=True,
        no_rewrite_rule_ids=True,
        strict=strict,
        optimizations=optimizations,
        autofix=True,
    )

    with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
        results = pool.starmap(invoke_semgrep_with_autofix_fn, config_with_tempfiles)

    fixtest_comparisons = {
        temp_copies[target]: fixtest
        for _config, testfiles in config_with_fixtests
        for target, fixtest in testfiles
    }

    fixtest_results: Dict[Path, Tuple[List[str], Path]] = {}
    fixtest_results_output = {}
    for t, tempcopy in temp_copies.items():
        fixtest = fixtest_comparisons[tempcopy]
        filediff = fixed_file_comparison(fixtest, tempcopy)
        # fixtest_results[t] = {"filediff": filediff, "fixtest": fixtest}
        fixtest_results[t] = (filediff, fixtest)
        fixtest_results_output[str(t)] = {"passed": len(filediff) == 0}
        os.remove(tempcopy)

    output = {
        "config_missing_tests": config_missing_tests_output,
        "config_missing_fixtests": configs_missing_fixtests,
        "config_with_errors": config_with_errors_output,
        "results": results_output,
        "fixtest_results": fixtest_results_output,
    }

    strict_error = bool(config_with_errors_output) and strict
    any_failures = any(
        not check_results["passed"]
        for file_results in results_output.values()
        for check_results in file_results["checks"].values()
    )

    any_fixtest_failures = any(
        not fixtest_file_results["passed"]
        for fixtest_file_results in fixtest_results_output.values()
    )

    exit_code = int(strict_error or any_failures or any_fixtest_failures)

    if json_output:
        print(json.dumps(output, indent=4, separators=(",", ": ")))
        sys.exit(exit_code)

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

    num_fixtests = 0
    num_fixtests_passed = 0
    fixtest_file_diffs: str = ""
    for target_filename, f_results in fixtest_results.items():
        num_fixtests += 1
        filediff = f_results[0]
        fixtest = f_results[1]
        if len(filediff) > 0:
            fixtest_file_diffs += _generate_fixcheck_output_line(
                target_filename, filediff, fixtest
            )
        else:
            num_fixtests_passed += 1

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

    if num_fixtests == 0:
        print("No tests for fixes found.")
    elif num_fixtests == num_fixtests_passed:
        print(f"{num_fixtests_passed}/{num_fixtests}: ✓ All fix tests passed ")
    else:
        print(
            f"{num_fixtests_passed}/{num_fixtests}: {num_fixtests - num_fixtests_passed} fix tests did not pass: "
        )
        print(BREAK_LINE)
        print(fixtest_file_diffs)

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
    optimizations: str,
    engine_type: EngineType,
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
        engine_type=engine_type,
        optimizations=optimizations,
    )
