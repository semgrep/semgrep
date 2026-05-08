#!/usr/bin/env python3
#
# Generate JSON and OCaml code describing the list of programming
# languages supported by Semgrep.
#
# Edit this file to add or modify a language, then run 'make' to
# update the files used by our various git projects.
#
###########################################################################
# Design notes:
#
# Initially, Martin wanted to use ATD because it defines a type (better than
# the readme). He tried that 6 months ago or so but didn't continue the
# effort because of the complexity.
#
# We need the following:
#
# 1. Generate type definitions for OCaml, Python, etc. that correspond to
#    each language.
# 2. We need to take the list of languages (data) and turn
#    it into code that OCaml, Python, TypeScript, etc. can use.
#
# Step 1 can be done with ATD well as we've done before in other
# contexts. Step 2 is done in Python (pysemgrep) by loading a JSON
# file at runtime. For OCaml (semgrep-core), it was done by an extra
# build step that takes the JSON data and turns it into OCaml (using
# jinja). I wanted to eliminate the jinja build step which is weird to
# have in an OCaml project (extra setup, needs extra expertise). Dune
# also buries the generated OCaml files (same problem with atdgen
# btw), so we end up without easy access to the contents of Lang.mli.
#
# To make the list of languages more accessible, the only approach I
# could see is to generate the OCaml files ahead of time, in a step
# outside of the dune build. This could be done either in the
# semgrep-langs repo or in the semgrep repo. Either would be fine as
# far as OCaml is concerned.
#
# However, we also need to produce a lang.json or equivalent Python and
# TypeScript code that contains the list of languages. Doing this with
# ATD involves (a) generating the target code in OCaml, Python, and
# TypeScript, and (b) running a program that uses that code and defines
# the list of languages. That program could be written in OCaml but it
# requires an opam setup. It could be written in Python, which is more
# likely to be installed on the user's machine. That's the solution I
# went for, thinking nobody would complain that we depend on a bunch of
# Python packages (in fact, the dependencies are rather lightweight and
# don't involve 3rd-party packages like atdgen-runtime for OCaml).
#
# Now, given the solution of writing a Python program to generate
# lang.json, we might as well define the type definitions (which are
# about 15 lines of code) in Python/mypy directly rather than ATD and
# duplicate it manually for OCaml. This is what's being done right
# now. It avoids the atdpy step and introduces a tiny bit of duplication
# of type definitions (somewhere, we print() an OCaml type definition
# similar to the mypy type definition of a language). Writing lang.json
# is the other part of the code where atdpy-generated Python code would
# have saved a bit of boilerplate. atdgen doesn't provide a way to write
# data in OCaml format like we wanted without going through JSON, so
# that's not an effort we could avoid.
#
# In conclusion, having a single generate script that only depends on a
# vanilla installation of Python 3 is simpler than ATD to set up for
# everyone who will have to touch this. ATD wouldn't make it easier to
# generate the list of languages in OCaml syntax, and Python is fine for
# this task. If the atd suite provided a direct way to generate data for
# all the target languages instead of going through JSON, we would
# certainly use it.


from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, TextIO
import json
import re


# Level of support for a language in Semgrep. Refer to external documents
# for their exact meaning e.g. https://semgrep.dev/docs/supported-languages/
#
# Constructors are sorted by increasing maturity to facilitate meaningful
# sorting.
#
class Maturity(Enum):
    # "develop" is not displayed in documentation
    DEVELOP = "develop"
    # "alpha" is now displayed as "experimental"
    ALPHA = "alpha"
    BETA = "beta"
    # GA means "general availability"
    GA = "ga"

    def to_json(self):
        return self.value


@dataclass
class Language():
    ##################################################################
    # Properties of the language as understood by a user
    ##################################################################

    # Machine-friendly unique language identifier
    id_: str

    # Human-readable name
    name: str

    # Names by which the language or analyzer can be referred to in
    # Semgrep (rules, command line, etc.)
    keys: List[str]
    maturity: Maturity

    # Comment for programmers who will read the generated code.
    comment: Optional[str] = None

    # Extension used when generating temporary or downloable files from
    # code snippets, hinting at what the file contains.
    example_ext: Optional[str] = None

    ##################################################################
    # Properties used to guess the language of a file.
    ##################################################################
    # This is useful for guessing purposes.
    # However, these properties are too simplistic to fully determine
    # target filtering as done by 'semgrep scan'.

    # File extensions that are commonly used for the language, including
    # the period.
    # Several languages may share an extension unlike for reverse_exts.
    # TODO: find out and explain why "" is a valid extension and why
    #       [""] works but [] doesn't. (I suspect it's an implementation
    #       quirk that should be fixed)
    # TODO: document what qualifies as an extension since don't seem to
    # they have to start with a ".".
    # TODO: document if they're case-sensitive.
    exts: List[str] = field(default_factory=lambda: [""])

    # File extensions that semgrep skips even if one of exts is a suffix,
    # such as ".min.js".
    excluded_exts: List[str] = field(default_factory=list)

    # If present, overrides exts when determining what language a
    # file is, otherwise defaults to exts; reverse_exts must be a one-to-one
    # mapping to languages.
    # TODO: clarify, give examples, and explain in which context it's usable.
    reverse_exts: Optional[List[str]] = None

    # Programs that execute scripts written in this language. May be empty.
    # TODO: this should not be defined here but where target
    # filtering takes place because target filtering is complicated.
    # Alternatively, explain your use case outside of 'semgrep scan'.
    shebangs: List[str] = field(default_factory=list)

    # Some "languages" are general-purpose text analyzers that aren't
    # programming languages or data formats. These include "regex" and
    # "aliengrep" which are not target languages. They do however have
    # their own pattern syntax and engine for matching patterns
    # against targets.
    is_target_language: bool = True

    # Mixed bag of tags used to select a subset of the languages.
    # Prefer boolean properties with a default value.
    # Current tags include:
    # - "is_proprietary" - for languages like Apex that not supported by
    #   the open-source version of Semgrep.
    # - "is_js" - for Javascript and Typescript
    # - "is_python" - for Python, Python2, Python3
    tags: List[str] = field(default_factory=list)

    def to_json(self):
        if self.comment:
            # Remove indentation from comment lines
            comment = re.sub("\n +", "\n", self.comment)
        else:
            comment = ""
        # eww
        res = {
            "comment": comment,
            "id": self.id_,
            "name": self.name,
            "keys": self.keys,
            "maturity": self.maturity.to_json(),
            "exts": self.exts,
            "example_ext": self.example_ext,
            "excluded_exts": self.excluded_exts,
            "reverse_exts": self.reverse_exts,
            "shebangs": self.shebangs,
            "is_target_language": self.is_target_language,
            "tags": self.tags,
        }
        if not self.comment:
            res.pop("comment", None)
        return res


LANGUAGES : List[Language] = [
    Language(
        comment="",
        id_="apex",
        name="Apex",
        keys=["apex"],
        exts=[".cls"],
        maturity=Maturity.DEVELOP,
        shebangs=[],
        tags=["is_proprietary"]
    ),
    Language(
        comment="",
        id_="bash",
        name="Bash",
        keys=["bash", "sh"],
        exts=[".bash", ".sh"],
        example_ext=".sh",
        maturity=Maturity.ALPHA,
        shebangs=["bash", "sh"]
    ),
    Language(
        comment="",
        id_="c",
        name="C",
        keys=["c"],
        exts=[".c", ".h"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="cairo",
        name="Cairo",
        keys=["cairo"],
        exts=[".cairo"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="circom",
        name="Circom",
        keys=["circom"],
        exts=[".circom"],
        maturity=Maturity.DEVELOP,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="clojure",
        name="Clojure",
        keys=["clojure"],
        exts=[".clj", ".cljs", ".cljc", ".edn" ],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="cpp",
        name="C++",
        keys=["cpp", "c++"],
        # Extensions are taken directly from tokei's language JSON file:
        #   https://github.com/XAMPPRocky/tokei/blob/master/languages.json
        # The .C was added for LSEG
        exts=[".cc", ".cpp", ".cxx", ".c++", ".pcc", ".tpp", ".C", ".h", ".hh", ".hpp", ".hxx", ".inl", ".ipp"],
        example_ext=".cpp",
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="csharp",
        name="C#",
        keys=["csharp", "c#"],
        exts=[".cs"],
        maturity=Maturity.GA,
        shebangs=[]
    ),
    Language(
        id_="dart",
        name="Dart",
        keys=["dart"],
        exts=[".dart"],
        maturity=Maturity.DEVELOP,
        shebangs=[]
    ),
    Language(
        comment="""'Dockerfile' is the only standard name for Dockerfiles.
The extension '.Dockerfile' is cited in the official documentation as
a popular extension. Whatever naming scheme is used in practice and is
not ambiguous is welcome here.
""",
        id_="dockerfile",
        name="Dockerfile",
        keys=["dockerfile", "docker"],
        # Extensions don't need start with a "."?
        exts=[".dockerfile", ".Dockerfile", "Dockerfile", "dockerfile"],
        example_ext=".dockerfile",
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="fga",
        name="Fga",
        keys=["fga", "openfga"],
        exts=[".fga"],
        example_ext=".fga",
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="elixir",
        name="Elixir",
        keys=["ex", "elixir"],
        exts=[".ex", ".exs"],
        maturity=Maturity.ALPHA,
        shebangs=[],
        tags=["is_proprietary"]
    ),
    Language(
        comment="",
        id_="go",
        name="Go",
        keys=["go", "golang"],
        exts=[".go"],
        maturity=Maturity.GA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="gosu",
        name="Gosu",
        keys=["gosu"],
        exts=[".gs"],
        maturity=Maturity.DEVELOP,
        shebangs=[],
        tags=["is_proprietary"],
    ),
    Language(
        comment="",
        id_="hack",
        name="Hack",
        keys=["hack"],
        exts=[".hack", ".hck", ".hh"],
        example_ext=".hack",
        maturity=Maturity.DEVELOP,
        shebangs=["hhvm"]
    ),
    Language(
        comment="",
        id_="html",
        name="HTML",
        keys=["html"],
        exts=[".htm", ".html"],
        example_ext=".html",
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="java",
        name="Java",
        keys=["java"],
        exts=[".java"],
        maturity=Maturity.GA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="js",
        name="JavaScript",
        keys=["js", "javascript"],
        exts=[".cjs", ".js", ".jsx", ".mjs"],
        excluded_exts=[".min.js"],
        example_ext=".jsx",
        maturity=Maturity.GA,
        shebangs=["node", "js", "nodejs"],
        tags=["is_js"]
    ),
    Language(
        comment="",
        id_="json",
        name="JSON",
        keys=["json"],
        exts=[".json", ".ipynb"],
        maturity=Maturity.GA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="jsonnet",
        name="Jsonnet",
        keys=["jsonnet"],
        exts=[".jsonnet", ".libsonnet"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="julia",
        name="Julia",
        keys=["julia"],
        exts=[".jl"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="kotlin",
        name="Kotlin",
        keys=["kt", "kotlin"],
        exts=[".kt", ".kts", ".ktm"],
        example_ext=".kt",
        maturity=Maturity.BETA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="lisp",
        name="Lisp",
        keys=["lisp"],
        exts=[".lisp", ".cl", ".el"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="lua",
        name="Lua",
        keys=["lua"],
        exts=[".lua"],
        maturity=Maturity.ALPHA,
        shebangs=["lua"]
    ),
    Language(
        comment="Move language with SUI flavor",
        id_="move_on_sui" ,
        name="Move on Sui",
        keys=["move_on_sui"],
        exts=[".move"],
        maturity=Maturity.DEVELOP,
        shebangs=[]
    ),
    Language(
        comment="Move language with Aptos flavor",
        id_="move_on_aptos",
        name="Move on Aptos",
        keys=["move_on_aptos"],
        exts=[".move"],
        maturity=Maturity.DEVELOP,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="ocaml",
        name="OCaml",
        keys=["ocaml"],
        exts=[".ml", ".mli"],
        example_ext=".ml",
        maturity=Maturity.ALPHA,
        shebangs=["ocaml", "ocamlscript"]
    ),
    Language(
        comment="",
        id_="php",
        name="PHP",
        keys=["php"],
        exts=[".php", ".tpl", ".phtml"],
        maturity=Maturity.GA,
        shebangs=["php"]
    ),
    Language(
        comment="",
        id_="powershell",
        name="Powershell",
        keys=["powershell"],
        exts=[".ps1"],
        maturity=Maturity.ALPHA,
    ),
    Language(
        comment="",
        id_="promql",
        name="Prometheus Query Language",
        keys=["promql"],
        exts=[".promql"],
        maturity=Maturity.ALPHA,
    ),
    Language(
        comment="",
        id_="protobuf",
        name="Protocol Buffers",
        keys=["proto", "protobuf", "proto3"],
        exts=[".proto"],
        maturity=Maturity.DEVELOP,
    ),
    Language(
        comment="",
        id_="python2",
        name="Python 2",
        keys=["python2"],
        exts=[".py", ".pyi"],
        # need to avoid conflict with Python3 and Python:
        reverse_exts=[],
        example_ext=".py",
        maturity=Maturity.DEVELOP,
        shebangs=["python", "python2"],
        tags=["is_python"]
    ),
    Language(
        comment="",
        id_="python3",
        name="Python 3",
        keys=["python3"],
        exts=[".py", ".pyi"],
        # need to avoid conflict with Python2 and Python:
        reverse_exts=[],
        example_ext=".py",
        maturity=Maturity.DEVELOP,
        shebangs=["python", "python3"],
        tags=["is_python"]
    ),
    Language(
        comment="",
        id_="python",
        name="Python",
        keys=["py", "python"],
        exts=[".py", ".pyi"],
        example_ext=".py",
        maturity=Maturity.GA,
        shebangs=["python", "python2", "python3"],
        tags=["is_python"]
    ),
    Language(
        comment="",
        id_="ql",
        name="QL",
        keys=["ql"],
        exts=[".ql", ".qll"],
        example_ext=".ql",
        maturity=Maturity.ALPHA,
    ),
    Language(
        comment="",
        id_="r",
        name="R",
        keys=["r"],
        exts=[".r", ".R"],
        example_ext=".R",
        maturity=Maturity.ALPHA
    ),
    Language(
        comment="",
        id_="ruby",
        name="Ruby",
        keys=["ruby"],
        exts=[".rb"],
        maturity=Maturity.GA,
        shebangs=["ruby"]
    ),
    Language(
        comment="",
        id_="rust",
        name="Rust",
        keys=["rust"],
        exts=[".rs"],
        maturity=Maturity.ALPHA,
        shebangs=["run-cargo-script"]
    ),
    Language(
        comment="",
        id_="scala",
        name="Scala",
        keys=["scala"],
        exts=[".scala"],
        maturity=Maturity.GA,
        shebangs=["scala"]
    ),
    Language(
        comment="",
        id_="scheme",
        name="Scheme",
        keys=["scheme"],
        exts=[".scm", ".ss"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="solidity",
        name="Solidity",
        keys=["solidity", "sol"],
        exts=[".sol"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="swift",
        name="Swift",
        keys=["swift"],
        exts=[".swift"],
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="terraform",
        name="Terraform",
        keys=["tf", "hcl", "terraform"],
        exts=[".tf", ".hcl", ".tfvars"],
        maturity=Maturity.GA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="ts",
        name="TypeScript",
        keys=["ts", "typescript"],
        exts=[".ts", ".tsx"],
        excluded_exts=[".d.ts"],
        example_ext=".tsx",
        maturity=Maturity.GA,
        shebangs=["ts-node"],
        tags=["is_js"]
    ),
    Language(
        comment="",
        id_="vue",
        name="Vue",
        keys=["vue"],
        exts=[".vue"],
        maturity=Maturity.DEVELOP,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="xml",
        name="XML",
        keys=["xml"],
        exts=[".xml", ".plist"],
        example_ext=".xml",
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="",
        id_="yaml",
        name="YAML",
        keys=["yaml"],
        exts=[".yml", ".yaml"],
        example_ext=".yaml",
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    ########################################################################
    # Entries that are not programming languages or data formats
    ########################################################################
    #
    # They're included here for legacy reasons.
    #
    Language(
        comment="""This can be used in rules as a target selector that selects
        all the files regardless of their extension or contents.
        When no target analyzer is specified, the spacegrep engine shall
        be used.
        """,
        id_="generic",
        name="Generic",
        is_target_language=False,
        keys=["generic", "spacegrep"],
        # Since "generic" is not a target selector, it doesn't
        # make sense to specify this 'exts' field. Specifying an empty
        # extension shouldn't be needed. We should remove it when once we're
        # confident that no implementation relies on it.
        exts=[""],
        example_ext=".generic",
        maturity=Maturity.ALPHA,
        shebangs=[]
    ),
    Language(
        comment="""Alternative engine for generic files""",
        id_="aliengrep",
        name="Aliengrep",
        is_target_language=False,
        keys=["aliengrep"],
        exts=[""],
        # need to avoid conflict with "generic":
        reverse_exts=[],
        maturity=Maturity.DEVELOP,
        shebangs=[]
    ),
    Language(
        comment="""This can be used in rules as a target selector that selects
        all the files regardless of their extension or contents.
        When no target analyzer is specified, the regex engine shall be used.
        """,
        id_="regex",
        name="regex",
        is_target_language=False,
        keys=["regex", "none"],
        exts=[""],
        # need to avoid conflict with "generic":
        reverse_exts=[],
        maturity=Maturity.DEVELOP
    ),
]


def write_ocaml_type_definitions(languages: List[Language], out: TextIO) -> None:
    out.write("""
(* All the programming languages for which Semgrep has dedicated support. *)
type t =
""")
    for x in languages:
        # OCaml syntax requires this format: [A-Z][A-Za-z0-9_]*
        variant_name = x.id_.capitalize()
        out.write(f"| {variant_name}\n")
    out.write("""
(*
   Maturity of the support for the programming language as shown to the
   public. The constructors are sorted by increasing maturity, allowing
   meaningful sorting using the default 'compare'.
*)
type maturity =
| Develop
| Alpha
| Beta
| Ga

(*
   Information about a supported programming language for which we have
   a dedicated parser (target analyzer). Some of this information can also be
   used for the purpose of target selection.
*)
type info = {
  id: t;
  id_string: string;
  name: string;
  keys: string list;
  exts: string list;
  maturity: maturity;
  example_ext: string option;
  excluded_exts: string list;
  reverse_exts: string list option;
  shebangs: string list;
  tags: string list;
}
""")


# Warning: not proper escaping but will work as long as the input string
# doesn't contain '|}'.
def ocaml_string(x: str) -> str:
    return "{|" + x + "|}"


def ocaml_string_option(x: Optional[str]) -> str:
    if x is None:
        return "None"
    else:
        return f"Some {ocaml_string(x)}"


def ocaml_string_list(xs: List[str]) -> str:
    quoted = [ ocaml_string(x) for x in xs ]
    return f"""[{"; ".join(quoted)}]"""


# There should be a way to get this by combining ocaml_string, ocaml_list,
# and ocaml_option but it's just too much for me right now.
def ocaml_string_list_option(x: Optional[List[str]]) -> str:
    if x is None:
        return "None"
    else:
        return f"Some {ocaml_string_list(x)}"


# Used by semgrep-core and osemgrep
def generate_ocaml(languages: List[Language], outfile_no_ext: str) -> None:
    mli = f"{outfile_no_ext}.mli"
    ml = f"{outfile_no_ext}.ml"
    langs = [ x for x in languages if x.is_target_language ]
    with open(mli, "w") as out:
        out.write("(* Generated file. Do not edit. *)\n")
        write_ocaml_type_definitions(langs, out)
        out.write("""
(*
   List of all the programming languages for which Semgrep has dedicated
   support. This list is sufficient to produce fast lookup tables implementing
   to_string, of_string, etc.
*)
val list : info list
""")
    with open(ml, "w") as out:
        out.write("(* Generated file. Do not edit. *)\n")
        write_ocaml_type_definitions(langs, out)
        out.write("""
let list = [
""")
        for x in langs:
            if x.comment:
                comment_lines = x.comment.splitlines()
                out.write("(*\n")
                for line in comment_lines:
                    out.write(f"  {line.strip()}\n")
                out.write("*)\n")
            out.write(f"""{{
  id = {x.id_.capitalize()};
  id_string = "{x.id_}";
  name = "{x.name}";
  keys = {ocaml_string_list(x.keys)};
  exts = {ocaml_string_list(x.exts)};
  maturity = {x.maturity.value.capitalize()};
  example_ext = {ocaml_string_option(x.example_ext)};
  excluded_exts = {ocaml_string_list(x.excluded_exts)};
  reverse_exts = {ocaml_string_list_option(x.reverse_exts)};
  shebangs = {ocaml_string_list(x.shebangs)};
  tags = {ocaml_string_list(x.tags)};
}};
""")
        out.write("]\n")


# Used by (py)semgrep and by semgrep-app
def generate_json(languages: List[Language], outfile: str) -> None:
    dicts = [ x.to_json() for x in languages ]
    with open(outfile, "w") as out:
        json.dump(dicts, out, indent=2)


def main() -> None:
    generate_ocaml(LANGUAGES, "Language")
    generate_json(LANGUAGES, "lang.json")


# En voiture Simone
main()
