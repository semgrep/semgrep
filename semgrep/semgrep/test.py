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
import logging
import multiprocessing
import sys
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Set
from typing import Tuple

from semgrep.constants import YML_EXTENSIONS
from semgrep.semgrep_main import invoke_semgrep
from semgrep.util import partition

logger = logging.getLogger(__name__)


def normalize_rule_id(line: str) -> str:
    """
    given a line like `     # ruleid:foobar`
    or `      // ruleid:foobar`
    return `foobar`
    """
    return line.strip().split(":")[1].strip().split(" ")[0].strip()


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
        or "<!--ruleid:" in line
        or "<!-- ruleid:" in line
    )


def line_has_ok(line: str) -> bool:
    return "#ok:" in line or "# ok:" in line or "//ok:" in line or "// ok:" in line


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
    ok_lines: Dict[str, Dict[str, List[int]]] = collections.defaultdict(
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

                rule_in_line = line_has_rule(line)
                ok_in_line = line_has_ok(line)
                todo_in_line = line_has_todo_rule(line)
                todo_ok_in_line = line_has_todo_ok(line)

                if todo_in_line:
                    num_todo += 1
                if (not ignore_todo and todo_in_line) or rule_in_line:
                    comment_lines[test_file_resolved][normalize_rule_id(line)].append(
                        effective_line_num
                    )
                if (not ignore_todo and todo_in_line) or ok_in_line:
                    ok_lines[test_file_resolved][normalize_rule_id(line)].append(
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
            oked = set(ok_lines[file_path][check_id])
            ignored = set(ignore_lines[file_path])

            reported_oked_lines = oked.intersection(all_reported)
            if reported_oked_lines:
                raise Exception(
                    f"found results on ok'ed lines - lines={reported_oked_lines} path={file_path}"
                )

            reported = all_reported - ignored

            new_cm = compute_confusion_matrix(reported, expected, oked)
            logger.debug(
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
    return f"TP: {tp}\tTN: {tn}\tFP: {fp}\tFN: {fn}"


def invoke_semgrep_multi(filename, *args, **kwargs):
    try:
        output = invoke_semgrep(filename, *args, **kwargs)
    except Exception as error:
        return (filename, error, {})
    else:
        return (filename, None, output)


def generate_file_pairs(
    location: Path, ignore_todo: bool, strict: bool, unsafe: bool
) -> None:
    print("starting tests...")

    filenames = list(location.rglob("*"))
    config_filenames = [
        filename
        for filename in filenames
        if filename.suffix in YML_EXTENSIONS
        and not filename.name.startswith(".")
        and not filename.parent.name.startswith(".")
    ]
    config_test_filenames = {
        config_filename: [
            inner_filename
            for inner_filename in filenames
            if inner_filename.with_suffix("") == config_filename.with_suffix("")
            and inner_filename.is_file()
            and inner_filename.suffix not in YML_EXTENSIONS
        ]
        for config_filename in config_filenames
    }
    config_with_tests, config_without_tests = partition(
        lambda c: c[1], config_test_filenames.items()
    )
    if config_without_tests:
        print("The following config files are missing tests:")
        print("\t" + "\n\t".join(str(c[0]) for c in config_without_tests))

    invoke_semgrep_fn = functools.partial(
        invoke_semgrep_multi,
        no_git_ignore=True,
        no_rewrite_rule_ids=True,
        strict=strict,
        dangerously_allow_arbitrary_code_execution_from_rules=unsafe,
        testing=True,
    )
    with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
        results = pool.starmap(invoke_semgrep_fn, config_with_tests)

    config_with_errors, config_without_errors = partition(lambda r: r[1], results)
    if config_with_errors:
        print("The following config files produced errors:")
        print(
            "\t"
            + "\n\t".join(
                f"{filename}: {error}" for filename, error, _ in config_with_errors
            )
        )
        if strict:
            sys.exit(1)

    tested = [
        (
            filename,
            score_output_json(output, config_test_filenames[filename], ignore_todo),
        )
        for filename, _, output in config_without_errors
    ]

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


def test_main(args: argparse.Namespace) -> None:
    _test_compute_confusion_matrix()

    if len(args.target) != 1:
        raise Exception("only one target directory allowed for tests")
    target = Path(args.target[0])

    generate_file_pairs(
        target,
        args.test_ignore_todo,
        args.strict,
        args.dangerously_allow_arbitrary_code_execution_from_rules,
    )
