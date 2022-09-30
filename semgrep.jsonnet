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

local all = yml.rules + semgrep_rules + pfff.rules + ocaml.rules;
{ rules: all }
