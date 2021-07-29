"""
For each directory containing YAML rules, run those rules on the file in the same directory with the same name but different extension.
E.g. eqeq.yaml runs on eqeq.py.
Validate that the output is annotated in the source file with by looking for a comment like:

 ```
 # ruleid:eqeq-is-bad
 ```
 On the preceeding line.

 """
import argparse
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
from typing import Set
from typing import Tuple

from semgrep.constants import BREAK_LINE
from semgrep.semgrep_main import invoke_semgrep
from semgrep.util import is_config_suffix
from semgrep.util import is_config_test_suffix
from semgrep.util import partition
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

EXIT_FAILURE = 1


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


def compute_confusion_matrix(
    reported: Set[Any], expected: Set[Any], oked: Set[Any]
) -> Tuple[int, int, int, int]:
    true_positives = len(expected.intersection(reported))
    false_positives = len(reported - expected)
    true_negatives = len(oked)
    false_negatives = len(expected - reported)

    return (true_positives, true_negatives, false_positives, false_negatives)


def _test_compute_confusion_matrix() -> None:
    tp, tn, fp, fn = compute_confusion_matrix(set([1, 2, 3, 4]), set([1]), set())
    assert tp == 1
    assert tn == 0
    assert fp == 3
    assert fn == 0

    tp, tn, fp, fn = compute_confusion_matrix(
        set([1, 2, 3, 4]), set([1, 2, 3, 4]), set([1])
    )
    assert tp == 4
    assert tn == 1
    assert fp == 0
    assert fn == 0

    tp, tn, fp, fn = compute_confusion_matrix(
        set([2, 3]), set([1, 2, 3, 4]), set([7, 8])
    )
    assert tp == 2
    assert tn == 2
    assert fp == 0
    assert fn == 2


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


def score_output_json(
    json_out: Dict[str, Any], test_files: List[Path], ignore_todo: bool
) -> Tuple[Dict[str, List[int]], Dict[str, Dict[str, Any]], int]:
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
    score_by_checkid: Dict[str, List[int]] = collections.defaultdict(
        lambda: [0, 0, 0, 0]
    )
    matches_by_check_id: Dict[str, Dict[str, Any]] = collections.defaultdict(dict)
    num_todo = 0

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
            num_todo += int(todo_rule_in_line) + int(todo_ok_in_line)

            try:
                if (not ignore_todo and todo_rule_in_line) or rule_in_line:
                    rule_ids = normalize_rule_ids(line)
                    for rule_id in rule_ids:
                        ruleid_lines[test_file_resolved][rule_id].append(
                            effective_line_num
                        )
                if (not ignore_todo and todo_rule_in_line) or ok_in_line:
                    rule_ids = normalize_rule_ids(line)
                    for rule_id in rule_ids:
                        ok_lines[test_file_resolved][rule_id].append(effective_line_num)
                if ignore_todo and todo_ok_in_line:
                    rule_ids = normalize_rule_ids(line)
                    for rule_id in rule_ids:
                        todo_ok_lines[test_file_resolved][rule_id].append(
                            effective_line_num
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

    rule_id_mismatch = False
    if reported_lines:
        for file_path, test_ids in test_lines.items():
            reported_ids = set(reported_lines[file_path].keys())
            if test_ids.symmetric_difference(reported_ids):
                logger.error(
                    f"found rule id mismatch - file={file_path} results={reported_ids} expected={test_ids}"
                )
                rule_id_mismatch = True

    if rule_id_mismatch:
        logger.error("failing due to rule id mismatch")
        sys.exit(EXIT_FAILURE)

    def join_keys(a: Dict[str, Any], b: Dict[str, Any]) -> Set[str]:
        return set(a.keys()).union(set(b.keys()))

    false_positive_lines = False
    for file_path in join_keys(ruleid_lines, reported_lines):
        for check_id in join_keys(ruleid_lines[file_path], reported_lines[file_path]):
            all_reported = set(reported_lines[file_path][check_id])
            expected = set(ruleid_lines[file_path][check_id])
            oked = set(ok_lines[file_path][check_id])
            todo_oked = set(todo_ok_lines[file_path][check_id])

            reported_oked_lines = oked.intersection(all_reported)
            if reported_oked_lines:
                logger.error(
                    f"found false positives on ok'ed lines - file={file_path} fps={reported_oked_lines}"
                )
                false_positive_lines = True

            reported = all_reported - todo_oked

            new_cm = compute_confusion_matrix(reported, expected, oked)
            matches_by_check_id[check_id][file_path] = {
                "expected_lines": sorted(expected),
                "reported_lines": sorted(reported),
            }
            old_cm = score_by_checkid[check_id]
            score_by_checkid[check_id] = [
                old_cm[i] + new_cm[i] for i in range(len(new_cm))
            ]

    if false_positive_lines:
        logger.error("failing due to false positives")
        sys.exit(EXIT_FAILURE)

    return (score_by_checkid, matches_by_check_id, num_todo)


def generate_confusion_string(check_results: Mapping[str, Any]) -> str:
    confusion_tp = f"TP: {check_results['tp']}"
    confusion_tn = f"TN: {check_results['tn']}"
    confusion_fp = f"FP: {check_results['fp']}"
    confusion_fn = f"FN: {check_results['fn']}"
    return f"{confusion_tp} {confusion_tn} {confusion_fp} {confusion_fn}"


def generate_check_output_line(check_id: str, check_results: Mapping[str, Any]) -> str:
    status = "✔" if check_results["passed"] else "✖"
    return f"\t{status} {check_id.ljust(60)} {generate_confusion_string(check_results)}"


def generate_matches_line(check_results: Mapping[str, Any]) -> str:
    def _generate_line(test_file: Any, matches: Mapping[str, Any]) -> str:
        test = f"test: {test_file}"
        expected = f"expected lines: {matches['expected_lines']}"
        reported = f"reported lines: {matches['reported_lines']}"
        return f"{test}, {expected}, {reported}"

    return "\t" + "\t\n".join(
        _generate_line(test_file, matches)
        for test_file, matches in check_results["matches"].items()
    )


def invoke_semgrep_multi(
    config: Path, targets: List[Path], **kwargs: Any
) -> Tuple[Path, Optional[Exception], Any]:
    try:
        output = invoke_semgrep(config, targets, **kwargs)
    except Exception as error:
        return (config, error, {})
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
    targets = list(original_target.rglob("*"))

    def target_matches_config(target: Path, config: Path) -> bool:
        correct_suffix = is_config_test_suffix(target) or not is_config_suffix(target)
        return (
            relatively_eq(original_target, target, original_config, config)
            and target.is_file()
            and correct_suffix
        )

    return {
        config: [target for target in targets if target_matches_config(target, config)]
        for config in configs
    }


def generate_file_pairs(
    target: Path,
    config: Path,
    ignore_todo: bool,
    strict: bool,
    unsafe: bool,
    json_output: bool,
    save_test_output_tar: bool = True,
    optimizations: str = "none",
) -> None:
    config_filenames = get_config_filenames(config)
    config_test_filenames = get_config_test_filenames(config, config_filenames, target)
    config_with_tests, config_without_tests = partition(
        lambda c: c[1], config_test_filenames.items()
    )
    config_missing_tests_output = [str(c[0]) for c in config_without_tests]

    invoke_semgrep_fn = functools.partial(
        invoke_semgrep_multi,
        no_git_ignore=True,
        no_rewrite_rule_ids=True,
        strict=strict,
        dangerously_allow_arbitrary_code_execution_from_rules=unsafe,
        optimizations=optimizations,
    )
    with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
        results = pool.starmap(invoke_semgrep_fn, config_with_tests)

    config_with_errors, config_without_errors = partition(lambda r: r[1], results)
    config_with_errors_output = [
        {"filename": str(filename), "error": str(error), "output": output}
        for filename, error, output in config_with_errors
    ]

    tested = {
        filename: score_output_json(
            output, config_test_filenames[filename], ignore_todo
        )
        for filename, _, output in config_without_errors
    }

    results_output: Mapping[str, Mapping[str, Any]] = {
        str(filename): {
            "todo": todo,
            "checks": {
                check_id: {
                    "tp": tp,
                    "tn": tn,
                    "fp": fp,
                    "fn": fn,
                    "passed": (fp == 0) and (fn == 0),
                    "matches": matches[check_id],
                }
                for check_id, (tp, tn, fp, fn) in output.items()
            },
        }
        for filename, (output, matches, todo) in tested.items()
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

    if config_missing_tests_output:
        print("The following config files are missing tests:")
        print("\t" + "\n\t".join(config_missing_tests_output))

    if config_with_errors_output:
        print("The following config files produced errors:")
        print(
            "\t"
            + "\n\t".join(
                f"{c['filename']}: {c['error']}" for c in config_with_errors_output
            )
        )

    # Place failed and TODO tests at the bottom for higher visibility
    passed_results_first = collections.OrderedDict(
        sorted(
            results_output.items(),
            key=lambda t: any(
                not c["passed"] or t[1]["todo"] for c in t[1]["checks"].values()
            ),
        )
    )

    print(f"{len(tested)} yaml files tested")
    print("check id scoring:")
    print(BREAK_LINE)

    totals: Dict[str, Any] = collections.defaultdict(int)

    for filename, rr in passed_results_first.items():
        print(f"(TODO: {rr['todo']}) {filename}")
        for check_id, check_results in sorted(rr["checks"].items()):
            print(generate_check_output_line(check_id, check_results))
            if not check_results["passed"]:
                print(generate_matches_line(check_results))
            for confusion in ["tp", "tn", "fp", "fn"]:
                totals[confusion] += check_results[confusion]

    print(BREAK_LINE)
    print(f"final confusion matrix: {generate_confusion_string(totals)}")
    print(BREAK_LINE)

    sys.exit(exit_code)


def test_main(args: argparse.Namespace) -> None:
    _test_compute_confusion_matrix()

    if len(args.target) != 1:
        raise Exception("only one target directory allowed for tests")
    target = Path(args.target[0])

    if args.config:
        if len(args.config) != 1:
            raise Exception("only one config directory allowed for tests")
        config = Path(args.config[0])
    else:
        config = target

    generate_file_pairs(
        target,
        config,
        args.test_ignore_todo,
        args.strict,
        args.dangerously_allow_arbitrary_code_execution_from_rules,
        args.json,
        args.save_test_output_tar,
        args.optimizations,
    )
