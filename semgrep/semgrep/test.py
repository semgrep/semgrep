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
import json
import sys
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Set
from typing import Tuple

from semgrep.constants import YML_EXTENSIONS
from semgrep.semgrep_main import main as semgrepmain
from semgrep.util import debug_print


def normalize_rule_id(line: str) -> str:
    """
    given a line like `     # ruleid:foobar`
    or `      // ruleid:foobar`
    return `foobar`
    """
    return line.strip().split(":")[1].strip()


def compute_confusion_matrix(
    reported: Set[Any], expected: Set[Any]
) -> Tuple[int, int, int, int]:
    true_positives = len(expected.intersection(reported))
    false_positives = len(reported - expected)
    true_negatives = 0  # we have no way to label "ok"
    false_negatives = len(expected - reported)

    return (true_positives, true_negatives, false_positives, false_negatives)


def _test_compute_confusion_matrix() -> None:
    tp, tn, fp, fn = compute_confusion_matrix(set([1, 2, 3, 4]), set([1]))
    assert tp == 1
    assert tn == 0
    assert fp == 3
    assert fn == 0

    tp, tn, fp, fn = compute_confusion_matrix(set([1, 2, 3, 4]), set([1, 2, 3, 4]))
    assert tp == 4
    assert tn == 0
    assert fp == 0
    assert fn == 0

    tp, tn, fp, fn = compute_confusion_matrix(set([2, 3]), set([1, 2, 3, 4]))
    assert tp == 2
    assert tn == 0
    assert fp == 0
    assert fn == 2


def line_has_todo_rule(line: str) -> bool:
    return (
        "#todoruleid:" in line
        or "# todoruleid:" in line
        or "// todoruleid:" in line
        or "//todoruleid:" in line
    )


def line_has_rule(line: str) -> bool:
    return (
        "#ruleid:" in line
        or "# ruleid:" in line
        or "//ruleid:" in line
        or "// ruleid:" in line
    )


def line_has_todo_ok(line: str) -> bool:
    return (
        "#todook" in line
        or "# todook" in line
        or "// todook" in line
        or "//todook" in line
    )


def score_output_json(
    json_out: Dict[str, Any], test_files: List[Path], ignore_todo: bool
) -> Tuple[Dict[str, List[int]], Dict[str, Dict[str, Any]], int]:
    comment_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )
    reported_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )
    ignore_lines: Dict[str, List[int]] = collections.defaultdict(list)
    score_by_checkid: Dict[str, List[int]] = collections.defaultdict(
        lambda: [0, 0, 0, 0]
    )
    expected_reported_by_check_id: Dict[str, Dict[str, Any]] = collections.defaultdict(
        dict
    )
    num_todo = 0

    for test_file in test_files:
        test_file_resolved = str(test_file.resolve())
        with open(test_file_resolved) as fin:
            all_lines = fin.readlines()
            for i, line in enumerate(all_lines):
                # +1 because we are 0 based and semgrep output is not, plus skip the comment line
                effective_line_num = i + 2

                todo_in_line = line_has_todo_rule(line)
                todo_ok_in_line = line_has_todo_ok(line)
                if todo_in_line:
                    num_todo += 1
                if (not ignore_todo and todo_in_line) or line_has_rule(line):
                    comment_lines[test_file_resolved][normalize_rule_id(line)].append(
                        effective_line_num
                    )
                if ignore_todo and todo_ok_in_line:
                    ignore_lines[test_file_resolved].append(effective_line_num)

    for result in json_out["results"]:
        reported_lines[str(Path(result["path"]).resolve())][result["check_id"]].append(
            int(result["start"]["line"])
        )

    def join_keys(a: Dict[str, Any], b: Dict[str, Any]) -> Set[str]:
        return set(a.keys()).union(set(b.keys()))

    for file_path in join_keys(comment_lines, reported_lines):
        for check_id in join_keys(comment_lines[file_path], reported_lines[file_path]):
            all_reported = set(reported_lines[file_path][check_id])
            expected = set(comment_lines[file_path][check_id])
            ignored = set(ignore_lines[file_path])

            reported = all_reported - ignored

            new_cm = compute_confusion_matrix(reported, expected)
            debug_print(
                f"reported lines for check {check_id}: {sorted(reported)}, expected lines: {sorted(expected)} (ignored: {sorted(ignored)}, confusion matrix: {new_cm}"
            )
            expected_reported_by_check_id[check_id][file_path] = (expected, reported)
            # TODO: -- re-enable this
            # assert len(set(reported_lines[file_path][check_id])) == len(
            #    reported_lines[file_path][check_id]
            # ), f"for testing, please don't make rules that fire multiple times on the same line ({check_id} in {file_path} on lines {reported_lines[file_path][check_id]})"
            old_cm = score_by_checkid[check_id]
            score_by_checkid[check_id] = [
                old_cm[i] + new_cm[i] for i in range(len(new_cm))
            ]

    return (score_by_checkid, expected_reported_by_check_id, num_todo)


def confusion_matrix_to_string(confusion: List[int]) -> str:
    tp, tn, fp, fn = confusion[0], confusion[1], confusion[2], confusion[3]
    return f"TP: {tp}\tTN:{tn}\t FP: {fp}\t FN: {fn}"


def invoke_semgrep(
    strict: bool, test_files: List[Path], config: Path, unsafe: bool
) -> Any:
    return json.loads(
        semgrepmain(
            target=[str(t) for t in test_files],
            pattern="",
            lang="",
            config=str(config),
            no_rewrite_rule_ids=True,
            jobs=1,
            include=[],
            include_dir=[],
            exclude=[],
            exclude_dir=[],
            json_format=True,
            debugging_json=False,
            sarif=False,
            output_destination="",
            quiet=True,
            strict=strict,
            exit_on_error=False,
            autofix=False,
            dangerously_allow_arbitrary_code_execution_from_rules=unsafe,
        )
    )


def generate_file_pairs(
    location: Path, ignore_todo: bool, strict: bool, semgrep_verbose: bool, unsafe: bool
) -> None:
    filenames = list(location.rglob("*"))
    no_tests = []
    tested = []
    semgrep_error = []
    print("starting tests...")
    for filename in filenames:
        if (
            filename.suffix in YML_EXTENSIONS
            and not filename.name.startswith(".")
            and not filename.parent.name.startswith(".")
        ):
            # find all filenames that have the same name but not extension, or are in a folder with the same name as a the yaml file
            yaml_file_name_without_ext = filename.with_suffix("")

            children_test_files = [
                p
                for p in filenames
                if str(p.with_suffix("")) == (str(yaml_file_name_without_ext))
            ]
            # remove yaml files from the test lists
            test_files = [
                path
                for path in children_test_files
                if path.suffix not in YML_EXTENSIONS and path.is_file()
            ]
            if not len(test_files):
                no_tests.append(filename)
                continue
            # invoke semgrep
            try:
                output_json = invoke_semgrep(strict, test_files, filename, unsafe)
                tested.append(
                    (filename, score_output_json(output_json, test_files, ignore_todo))
                )
            except Exception as ex:
                print(
                    f"semgrep error running with config {filename} on {test_files}:\n{ex}"
                )
                semgrep_error.append(filename)

    if len(semgrep_error) and strict:
        print("exiting due to semgrep/config errors and strict flag")
        sys.exit(1)

    print(f"{len(no_tests)} yaml files missing tests")
    debug_print(f"missing tests: {no_tests}")
    print(f"{len(tested)} yaml files tested")
    print("check id scoring:")
    print("=" * 80)
    failed_tests = []
    total_confusion = [0, 0, 0, 0]

    for (filename, (output, expected_reported_by_check_id, num_todo)) in tested:
        print(filename)
        if not len(output.items()):
            print(f"  no checks fired (TODOs: {num_todo})")
        for check_id, (tp, tn, fp, fn) in output.items():
            good = (fp == 0) and (fn == 0)
            if not good:
                failed_tests.append(
                    (filename, check_id, expected_reported_by_check_id[check_id])
                )
            status = "✔" if good else "✖"
            todo_text = f"(TODOs: {num_todo})" if num_todo > 0 else ""
            confusion = [tp, tn, fp, fn]
            # add to the total confusion matrix
            total_confusion = [
                total_confusion[i] + confusion[i] for i in range(len(confusion))
            ]
            print(
                f"  {status} - {check_id.ljust(60)}{confusion_matrix_to_string(confusion)} {todo_text}"
            )

    print("=" * 80)
    print(f"final confusion matrix: {confusion_matrix_to_string(total_confusion)}")
    print("=" * 80)

    if len(failed_tests) > 0:
        print(f"failing rule files: ")
        for (filename, check_id, failed_test_files) in failed_tests:
            print(f" ✖ FAILED rule file: {filename} check: {check_id}")
            for test_file_path, (expected, reported) in failed_test_files.items():
                print(
                    f"              in test: {test_file_path}, expected lines: {sorted(expected)} != reported: {sorted(reported)}"
                )
        print(
            f"{len(failed_tests)} checks failed tests (run with verbose flag for more details)"
        )
        sys.exit(1)
    else:
        print("all tests passed")
        sys.exit(0)


def main(
    location: Path,
    ignore_todo: bool,
    verbose: bool,
    strict: bool,
    semgrep_verbose: bool,
    unsafe: bool,
) -> None:
    generate_file_pairs(location, ignore_todo, strict, semgrep_verbose, unsafe)


def test_main(args: argparse.Namespace) -> None:
    _test_compute_confusion_matrix()
    if len(args.target) != 1:
        raise Exception("only one target directory allowed for tests")
    target = Path(args.target[0])
    main(
        target,
        args.test_ignore_todo,
        args.verbose,
        args.strict,
        args.verbose,
        args.dangerously_allow_arbitrary_code_execution_from_rules,
    )
