# -*- jsonnet -*-

# legacy rules written in YAML
local yml = import "semgrep.yml";

# registry rules!
local ocaml = import "p/ocaml";

# this is useful to factorize exclude
local test_code_globs = ["Unit_*.ml", "Test_*.ml"];
local less_important_code_globs = ["spacegrep/", "experiments/", "scripts/"];

local semgrep_rules = [
#TODO: does not parse anymore!!
#  { id: "pfff-no-open-in",
#    match: { or: ["open_in_bin ...", "open_in ..."]},
#    # Same but using The old syntax:
#    #  "pattern-either": [
#    #    { pattern: "open_in_bin ..." },
#    #    { pattern: "open_in ..." },
#    #   ],
#    languages: ["ocaml"],
#    severity: "ERROR",
#    message: |||
#        It is easy to forget to close `open_in` with `close_in`.
#        Use `Common.with_open_infile()` instead.
#    |||,
#  }
];

local todo_skipped_for_now = [
  #TODO? what is the fix for that?
  "ocaml.lang.portability.crlf-support.broken-input-line",
];

local override_messages = {
  // semgrep specific adjustments
  "ocaml.lang.best-practice.exception.bad-reraise": |||
    You should not re-raise exceptions using 'raise' because it loses track of where the
    exception was raised originally. See commons/Exception.mli for more information.
    Use `Exception.catch exn` and later `Exception.raise exn` or
    `Exception.catch_and_reraise exn` if there is no code between the moment you
    catch the exn and re-raise it.
  |||,
};

#Temporary hack to not report p/ocaml findings on pfff libs
#TODO: we should use +: instead of :, as in
#  [ r + { paths +: { exclude +: [ "libs/*", "tools/*", "languages/*" ] } }
# but this is not supported yet by ojsonnet hence the use of :
local ocaml_rules =
  [ r + { paths : { exclude : [ "libs/*", "tools/*", "languages/*" ] } }
    for r in ocaml.rules];

local all = yml.rules + semgrep_rules + ocaml_rules;

  { rules:
      [  if std.objectHas(override_messages, r.id)
         then (r + {message: override_messages[r.id]})
         else r
        for r in all
        if !std.member(todo_skipped_for_now, r.id)
      ]
  }
