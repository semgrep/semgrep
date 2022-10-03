local yml = import "semgrep.yml";
local pfff = import "semgrep-core/src/pfff/semgrep.yml";
local ocaml = import "p/ocaml";

local semgrep_rules = [
  #TODO: this rule could be moved in pfff/semgrep.yml at some point
  { id: "pfff-no-open-in",
    match: { or: ["open_in_bin ...", "open_in ..."]},
    # Same but using The old syntax:
    #  "pattern-either": [
    #    { pattern: "open_in_bin ..." },
    #    { pattern: "open_in ..." },
    #   ],
    languages: ["ocaml"],
    severity: "ERROR",
    message: |||
        It is easy to forget to close `open_in` with `close_in`.
        Use `Common.with_open_infile()` instead.
    |||,
  }
];

#TODO: we should fix the offending code (or add a nosemgrep comment)
# instead of skipping those rules!
local todo_skipped_for_now = [
   "physical-inequality",
   "no-List-find-outside-try",
   "ocaml.lang.best-practice.ifs.ocamllint-useless-else",
   "ocaml.lang.correctness.physical_vs_structural.physical-equal",
   "ocaml.lang.best-practice.hashtbl.hashtbl-find-outside-try",
   "ocaml.lang.correctness.physical_vs_structural.physical-not-equal",
   "ocaml.lang.best-practice.ifs.ocamllint-backwards-if",
   "ocaml.lang.best-practice.string.ocamllint-str-first-chars",
   "ocaml.lang.best-practice.list.list-find-outside-try",
   "ocaml.lang.portability.crlf-support.broken-input-line",
   "ocaml.lang.correctness.physical-vs-structural.physical-equal",
   "ocaml.lang.correctness.physical-vs-structural.physical-not-equal",
  "ocaml.lang.best-practice.exception.bad-reraise",
];

local all = yml.rules + semgrep_rules + pfff.rules + ocaml.rules;
  { rules:
      [
	r for r in all
	if !std.member(todo_skipped_for_now, r.id)
      ]
  }
