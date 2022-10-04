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

local todo_skipped_for_now = [
  #TODO? what is the fix for that?
  "ocaml.lang.portability.crlf-support.broken-input-line",
];

local override_messages = {
  // pfff/semgrep specific adjustments
  "ocaml.lang.best-practice.exception.bad-reraise": |||
    You should not re-raise exceptions using 'raise' because it loses track of where the
    exception was raised originally. See pfff/commons/Exception.mli for more information.
    Use `Exception.catch exn` and later `Exception.raise exn` or
    `Exception.catch_and_reraise exn` if there is no code between the moment you
    catch the exn and re-raise it.
  |||,
};

local all = yml.rules + semgrep_rules + pfff.rules + ocaml.rules;

  { rules:
      [  if std.objectHas(override_messages, r.id)
	 then r + {message: override_messages[r.id]}
         else r
	for r in all
	if !std.member(todo_skipped_for_now, r.id)
      ]
  }
