#!/usr/bin/env python3
#
# Generate cheatsheets displayed next to the live editor, for each language.
#
# Usage:
#   $ ./scripts/generate-cheatsheet
#
# or more explicitly:
#   $ cd cli
#   $ pipenv run ../scripts/generate_cheatsheet.py --json --directory \
#     ../tests --output-file cheatsheet.json
#
#
# If you add support for a new language, you may need to update
# LANG_DIR_TO_EXT below.
#
# CHEATSHEET_ENTRIES defines the entries shown on the cheatsheet for each
# language, regardless of whether a pair (pattern, target) exists.
# The web UI will show a message like "missing example" where appropriate.
#
# For each example, the full name is created as CATEGORY_ENTRY, e.g.
# "dots_args" for the entry "args" in the category "dots".
#
# An example 'cat_ent' is valid for language 'lang' of extension '.ext'
# if two files are found:
# - cat_ent.sgrep
# - cat_ent.ext
# These files are searched for in those places:
# 1. In the language's folder 'lang/'.
# 2. In the 'POLYGLOT/' folder which is shared with other languages, as
#    a fallback.
#
# The POLYGLOT/ folder is meant for patterns that are identical in many
# languages so as to avoid unnecessary duplication.
#
import argparse
import collections
import glob
import io
import json
import multiprocessing
import os
import subprocess
import tempfile
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

from ruamel.yaml import YAML

yaml = YAML()


FEATURES = ["dots", "equivalence", "metavar", "misc"]

VERBOSE_FEATURE_NAME = {
    "dots": "Wildcard Matches (...)",
    "equivalence": "Helpful Features",
    "metavar": "Named Placeholders ($X)",
    "misc": "Others",
    "metavar_equality": "Reoccurring Expressions",
    "concrete": "Exact Matches",
    "regexp": "Regular Expressions '=~/regexp/'",
    "deep": "Deep (Recursive) Matching",
}

VERBOSE_SUBCATEGORY_NAME = {
    "stmt": "Statement",
    "stmts": "Statements",
    "call": "Function Call",
    "nested_stmts": "Nested Statements",
    "cond": "Conditionals",
    "arg": "Argument",
    "args": "Arguments",
    "import": "Imports",
    "string": "Strings",
    "expr": "Expressions",
    "var": "Variables",
    "naming_import": "Import Renaming/Aliasing",
    "constant_propagation": "Constant Propagation",
    "fieldname": "Field Names",
    "syntax": "Single Statements",
    "exprstmt": "Expression and Statement",
    "typed": "Typed Metavariables",
    "expr_operator": "Deep Expression Operator",
    "class_def": "Class Definitions",
    "anno": "Annotations",
    "func_def": "Function Definitions",
    "key_value": "Object or Dictionary Key Value Pairs",
    "typed_fieldaccess": "Typed Metavariable Field Access",
    "method_chaining": "Method Chaining",
}

LANGUAGE_EXCEPTIONS = {
    "java": ["naming_import", "key_value"],
    "c": ["naming_import", "class_def", "anno"],
    "ruby": ["naming_import", "typed", "anno"],
    "python": ["typed"],
    "js": ["typed"],
    "go": ["class_def", "anno"],
    "ocaml": ["anno", "key_value"],
}

EXCLUDE = ["TODO", "POLYGLOT", "e2e", "OTHER"]

#
# Entries shown on the cheatsheet for each language, regardless of whether
# a pair (pattern, target) exists.
#
# TODO: add dots_for, implemented in JS and Go now
CHEATSHEET_ENTRIES = {
    "concrete": ["syntax"],
    "dots": [
        "args",
        "string",
        "stmts",
        "nested_stmts",
        "method_chaining",
    ],
    "metavar": [
        "call",
        "arg",
        "stmt",
        "cond",
        "func_def",
        "class_def",
        "import",
        "typed",
        "anno",
        "key_value",
        "typed_fieldaccess",
    ],
    "regexp": ["string", "fieldname"],
    "metavar_equality": ["expr", "stmt", "var"],
    "equivalence": [
        "naming_import",
        # "field-order", TODO
        # "arg-order", TODO
        "constant_propagation",
    ],
    "deep": ["exprstmt", "expr_operator"],
}

ALPHA_FEATURES = {
    "concrete": ["syntax"],
    "dots": ["args", "string", "stmts", "nested_stmts"],
    "metavar": ["call", "arg"],
    "metavar_equality": ["var"],
    "deep": ["exprstmt"],
}

BETA_FEATURES = {
    "metavar": ["stmt", "cond", "import", "class_def", "func_def"],
    "metavar_equality": ["stmt", "expr"],
}

GA_FEATURES = {
    "metavar": ["typed", "anno", "key_value", "ellipsis_args"],
    "regexp": ["string"],
    "equivalence": [
        "naming_import",
        "constant_propagation",
    ],
    "deep": ["expr_operator"],
}

NUM_ALPHA_FEATURES = sum(len(val) for val in ALPHA_FEATURES.values())
NUM_BETA_FEATURES = sum(len(val) for val in BETA_FEATURES.values())
NUM_GA_FEATURES = sum(len(val) for val in GA_FEATURES.values())


def find_path(
    root_dir: str, lang: str, category: str, subcategory: str, extension: str
):
    base_path = os.path.join(root_dir, lang, f"{category}_{subcategory}")
    joined = base_path + "." + extension
    if os.path.exists(joined):
        return joined
    else:
        generic_base_path = os.path.join(
            root_dir, "POLYGLOT", f"{category}_{subcategory}"
        )
        joined = generic_base_path + "." + extension
        return joined


def _single_pattern_to_dict(pattern: str, language: str) -> Dict[str, Any]:
    pattern = pattern.strip()
    if len(pattern.split("\n")) > 1:
        pattern = (
            pattern + "\n"
        )  # make sure multi-line patterns end in new-line otherwise semgrep dies # TODO is this still true?

    sgrep_config_default: Dict[str, Any] = {
        "rules": [
            {
                "id": "default-example",
                "patterns": [{"pattern": pattern}],
                "message": "msg",
                "languages": [language],
                "severity": "WARNING",
            }
        ]
    }
    sgrep_config_default["rules"][0]["patterns"][0]["pattern"] = pattern
    return sgrep_config_default


def _config_to_string(config: Any) -> str:
    stream = io.StringIO()
    yaml.dump(config, stream)
    return stream.getvalue()


def run_semgrep_on_example(
    lang: str, config_arg_str: str, code_path: str
) -> Optional[dict]:
    with tempfile.NamedTemporaryFile("w") as config:
        pattern_text = open(config_arg_str).read()
        config.write(_config_to_string(_single_pattern_to_dict(pattern_text, lang)))
        config.flush()
        cmd = ["semgrep", "--strict", "--json", f"--config={config.name}", code_path]
        print(">>> " + " ".join(cmd))
        output = subprocess.run(  # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
            cmd,
            capture_output=True,
        )
        if output.returncode == 0:
            print(output.stderr.decode("utf-8"))
            return json.loads(output.stdout.decode("utf-8"))
        else:
            print("ERROR: " + str(output.returncode))
            print(cmd)
            return None


def invoke_semgrep_multi(semgrep_path, code_path, lang, category, subcategory):
    if paths_exist(semgrep_path, code_path):
        result = run_semgrep_on_example(lang, semgrep_path, code_path)
    else:
        result = {}
    return (
        semgrep_path,
        code_path,
        lang,
        category,
        subcategory,
        result,
    )


def paths_exist(*paths):
    return all(os.path.exists(path) for path in paths)


def generate_cheatsheet(root_dir: str, html: bool):
    # output : {'dots': {'arguments': ['foo(...)', 'foo(1)'], } }
    output = collections.defaultdict(
        lambda: collections.defaultdict(lambda: collections.defaultdict(list))
    )
    langs = get_language_directories(root_dir)
    semgrep_multi_args = [
        (
            find_path(root_dir, lang, category, subcategory, "sgrep"),
            find_path(root_dir, lang, category, subcategory, lang_dir_to_ext(lang)),
            lang,
            category,
            subcategory,
        )
        for lang in langs
        for category, subcategories in CHEATSHEET_ENTRIES.items()
        for subcategory in subcategories
    ]
    with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
        results = pool.starmap(invoke_semgrep_multi, semgrep_multi_args)

    for semgrep_path, code_path, lang, category, subcategory, result in results:
        highlights = []
        if result is None:
            raise Exception(
                f"rule '{code_path}' produced errors, please fix these before proceeding"
            )
        else:
            if "results" in result and not result["results"]:
                raise Exception(
                    f"rule '{code_path}' produced no findings and is useless, please fix or TODO before proceeding"
                )
            highlights.extend(
                {"start": r["start"], "end": r["end"]}
                for r in result.get("results", [])
            )

        entry = {
            "pattern": read_if_exists(semgrep_path),
            "pattern_path": os.path.relpath(semgrep_path, root_dir),
            "code": read_if_exists(code_path),
            "code_path": os.path.relpath(code_path, root_dir),
            "highlights": highlights,
        }

        if html:
            entry["pattern_path"] = os.path.relpath(semgrep_path)
            entry["code_path"] = os.path.relpath(code_path)

        feature_name = VERBOSE_FEATURE_NAME.get(category, category)
        subcategory_name = VERBOSE_SUBCATEGORY_NAME.get(subcategory, subcategory)
        feature_exception = feature_name in LANGUAGE_EXCEPTIONS.get(lang, [])
        subcategory_exception = subcategory in LANGUAGE_EXCEPTIONS.get(lang, [])
        if not feature_exception and not subcategory_exception:
            output[lang][feature_name][subcategory_name].append(entry)

    return output


CSS = """
.pattern {
    background-color: #0974d7;
    color: white;
    padding: 10px;
}

.match {
    background-color: white;
    padding: 10px;
    border: 1px solid #0974d7;
    color: black;
}

.pair {
    display: flex;
    width: 100%;
    font-family: Consolas, Bitstream Vera Sans Mono, Courier New, Courier, monospace;
    font-size: 1em;
}

.example {
    padding: 10px;
    margin: 10px;
    border: 1px solid #ccc;
}

.examples {
    display: flex;
}

a {
    text-decoration: none;
    color: inherit;
}

pre {
    margin: 0;
}

.example-category {
    width: fit-content;
    border-top: 1px solid #ddd;
}

.notimplemented {
    background-color: yellow;
}

h3 {
    margin: 0;
    margin-bottom: 10px;
}

table {
  border-collapse: collapse;
  width: 100%;
}

td, th {
  border: 1px solid #ddd;
  padding: 8px;
}

tr:nth-child(even){background-color: #f2f2f2;}

th {
  padding-top: 12px;
  padding-bottom: 12px;
  text-align: left;
  background-color: #0974d7;
  color: white;
}

"""


def snippet_and_pattern_to_html(
    sgrep_pattern: str, sgrep_path: str, code_snippets: List[Tuple[str, str]]
):
    s = ""
    if sgrep_pattern:
        s += f'<div class="pattern"><a href="{sgrep_path}"><pre>{sgrep_pattern}</pre></a></div>\n'
        if len([x for x in code_snippets if x[0]]):
            # replace < and > in snippets to be &lt and &gt so that the code snippets can render
            for i, code_snippet in enumerate(code_snippets):
                snippet, path = code_snippet
                snippet = snippet.replace("<", "&lt")
                snippet = snippet.replace(">", "&gt")
                code_snippets[i] = (snippet, path)

            snippets_html = "".join(
                [
                    f'<div class="match"><a href="{path}"><pre><code>{snippet}</code></pre></a></div>\n'
                    for snippet, path in code_snippets
                ]
            )
            s += f"<div>{snippets_html}</div>"
        else:
            return (
                f'<div class="notimplemented">This is missing an example!<br/>Or it doesn\'t work yet for this language!<br/>Edit {sgrep_path}</div>\n',
                False,
            )
    else:
        return (
            f'<div class="notimplemented">not implemented, no sgrep pattern at {sgrep_path}</div>\n',
            False,
        )
    return s, True


def wrap_in_div(L: List[str], className="") -> List[str]:
    return "".join([f"<div class={className}>{i}</div>" for i in L])


def add_headers_for_category(category: str, subcategories: List[str]) -> str:
    s = ""
    category_long = VERBOSE_FEATURE_NAME[category]
    for subcategory in subcategories:
        subcategory_long = VERBOSE_SUBCATEGORY_NAME[subcategory]
        s += f"<th>{category_long}:{subcategory_long}</th>"
    return s


def generate_headers_for_table():
    s = f'<tr><th></th><th colspan={NUM_ALPHA_FEATURES}" scope="colgroup">Alpha Features</th>'
    s += f'<th colspan="{NUM_BETA_FEATURES}" scope="colgroup">Beta Features</th>'
    s += f'<th colspan="{NUM_GA_FEATURES}" scope="colgroup">GA Features</th></tr>'
    s += "<tr><th></th>"

    for category, subcategories in ALPHA_FEATURES.items():
        s += add_headers_for_category(category, subcategories)

    for category, subcategories in BETA_FEATURES.items():
        s += add_headers_for_category(category, subcategories)

    for category, subcategories in GA_FEATURES.items():
        s += add_headers_for_category(category, subcategories)

    s += "</tr>"
    return s


def check_if_test_exists(
    test_matrix_dict: Dict[str, Dict[Tuple, bool]],
    category: str,
    subcategory: str,
    lang: str,
) -> str:
    test_matrix_entry = (
        VERBOSE_FEATURE_NAME[category],
        VERBOSE_SUBCATEGORY_NAME[subcategory],
    )
    if subcategory in LANGUAGE_EXCEPTIONS.get(lang, []):
        return f"<td>&#128125;</td>\n"
    if test_matrix_entry in test_matrix_dict[lang]:
        test_exists = test_matrix_dict[lang][test_matrix_entry]
        if test_exists:
            return f"<td>&#9989;</td>\n"
        else:
            return f"<td>&#10060;</td>\n"
    return f"<td>&#10060;</td>\n"


def generate_table(
    cheatsheet: Dict[str, Any], test_matrix_dict: Dict[str, Dict[Tuple, bool]]
) -> str:
    s = "<h2>Table of Languages and Features Supported</h2>"
    s += '<table class="pure-table pure-table-bordered"><tr>\n<td></td>\n'

    # get the feature headers in:
    s += generate_headers_for_table()

    # for each feature:
    for lang in cheatsheet.keys():
        s += "<tr>\n"
        s += f"<th>{lang}</th>\n"
        for category, subcategories in ALPHA_FEATURES.items():
            for subcategory in subcategories:
                s += check_if_test_exists(test_matrix_dict, category, subcategory, lang)

        for category, subcategories in BETA_FEATURES.items():
            for subcategory in subcategories:
                s += check_if_test_exists(test_matrix_dict, category, subcategory, lang)

        for category, subcategories in GA_FEATURES.items():
            for subcategory in subcategories:
                s += check_if_test_exists(test_matrix_dict, category, subcategory, lang)
        s += "</tr>\n"
    return s


def cheatsheet_to_html(cheatsheet: Dict[str, Any]):
    s = ""
    s += f"<head><style>{CSS}</style></head><body>"

    test_matrix_dict = collections.defaultdict(lambda: collections.defaultdict(bool))
    for lang, categories in cheatsheet.items():
        s += f"<h2>{lang}</h2>"
        for category, subcategories in categories.items():
            examples = []
            for subcategory, entries in subcategories.items():
                by_pattern = collections.defaultdict(list)
                for entry in entries:
                    by_pattern[(entry["pattern"], entry["pattern_path"])].append(
                        (entry["code"], entry["code_path"])
                    )

                html_snippets = [
                    snippet_and_pattern_to_html(pattern, pattern_path, snippets)
                    for (pattern, pattern_path), snippets in by_pattern.items()
                ]

                compiled_examples = [html_snippet[0] for html_snippet in html_snippets]

                test_matrix_dict[lang][(category, subcategory)] = html_snippets[0][1]

                html = wrap_in_div(compiled_examples, className="pair")
                examples.append(
                    f'<div class="example"><h3>{subcategory}</h3>{html}</div>'
                )
            s += f'<div class="example-category"><h2>{category}</h2><div class="examples">{"".join(examples)}</div></div>'
    s += generate_table(cheatsheet, test_matrix_dict)
    s += "</body>"
    return s


def read_if_exists(path: Optional[str]):
    if path and os.path.exists(path):
        text = str(open(path).read())
        return text


def lang_dir_to_ext(lang: str):
    LANG_DIR_TO_EXT = {
        "python": "py",
        "ruby": "rb",
        "ocaml": "ml",
        "csharp": "cs",
        "rust": "rs",
        "solidity": "sol",
        "elixir": "ex",
        "kotlin": "kt",
        "terraform": "tf",
        "julia": "jl",
    }
    return LANG_DIR_TO_EXT.get(lang, lang)


def get_emoji(count: int):
    if count == 0:
        return "\U0001F6A7"
    elif count < 5:
        return "\U0001F536"
    else:
        return "\U00002705"


def print_to_html(stats):
    def append_td(l, name):
        l.append("<td>")
        l.append(name)
        l.append("</td>")

    tags = ['<table style="text-align:center">', "<tr>"]
    languages = stats.keys()
    append_td(tags, "")
    for lang in languages:
        append_td(tags, f"<b>{lang}</b>")
    tags.append("</tr>")

    for f in FEATURES:
        tags.append("<tr>")
        append_td(tags, f"{VERBOSE_FEATURE_NAME.get(f)}")
        for lang in languages:
            append_td(tags, f"{get_emoji(stats[lang].get(f, 0))}")
        tags.append("</tr>")
    tags.append("</table>")
    return "\n".join(tags)


def compute_stats(dir_name: str, lang_dir: str):
    path = os.path.join(dir_name, lang_dir)
    count_per_feature = {}
    for f in FEATURES:
        count_per_feature[f] = len(
            glob.glob1(path, f"{f}*.{lang_dir_to_ext(lang_dir)}")
        )
    return count_per_feature


def get_language_directories(dir_name: str) -> List[str]:
    files = os.listdir(dir_name)
    return [
        f
        for f in files
        if os.path.isdir(os.path.join(dir_name, f)) and not f in EXCLUDE
    ]


def parse_args():
    p = argparse.ArgumentParser(
        description="""
        Generate cheatsheet for local viewing and semgrep-app usage.
        """,
        formatter_class=argparse.RawTextHelpFormatter,
    )

    p.add_argument(
        "-d",
        "--directory",
        action="store",
        required=True,
        help="analyze this directory of tests",
    )
    p.add_argument(
        "-o",
        "--output-file",
        action="store",
        type=argparse.FileType("w"),
        help="output to this file",
    )

    output_group = p.add_mutually_exclusive_group(required=True)
    output_group.add_argument("-j", "--json", action="store_true", help="output JSON")
    output_group.add_argument("-t", "--html", action="store_true", help="output HTML")

    args = p.parse_args()

    return args


def main() -> None:
    args = parse_args()

    all_subcategories = set(VERBOSE_SUBCATEGORY_NAME.keys())
    cheatsheet_subcategories = {
        subcategory
        for subcategory_list in CHEATSHEET_ENTRIES.values()
        for subcategory in subcategory_list
    }
    if all_subcategories != cheatsheet_subcategories:
        raise Exception(
            f"found subcategory mismatch, all={all_subcategories} cheatsheet={cheatsheet_subcategories}"
        )

    cheatsheet = generate_cheatsheet(args.directory, args.html)

    if args.json:
        output = json.dumps(
            cheatsheet, indent=2, separators=(",", ": "), sort_keys=True
        )
    elif args.html:
        output = cheatsheet_to_html(cheatsheet)

    if args.output_file:
        args.output_file.write(output)
    else:
        print(output)


if __name__ == "__main__":
    main()
