# legacy rules written in YAML
local yml = import "semgrep.yml";
local pfff = import "semgrep-core/src/pfff/semgrep.yml";
# registry rules!
local ocaml = import "p/ocaml";

# this is useful to factorize exclude
local test_code_globs = ["Unit_*.ml", "Test_*.ml"];
local less_important_code_globs = ["spacegrep/*", "experiments/*", "scripts/*"];

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
  },
  #TODO: this is an experimental rule. It has lots of findings, so maybe it is too
  # noisy, but it's nice to dogfood how to handle noisy rules.
  # 564 findings before the exclude
  { id: "semgrep-no-catch-all-match-underscore",
    match: "| _ -> $X",
    languages: ["ocaml"],
    severity: "WARNING",
    message: |||
        Using a catch all pattern is dangerous in the long term. If someone adds a new
	constructor, the compiler will not help us and telling us to maybe update this code.
        Try to replace with the list of the remaining cases not handled instead (you can
	rely on ocamlc to give you the disjunctive pattern covering all the cases and copy
	paste it in the code). You can also add a nosemgrep comment if you think
	you know what you're doing.
    |||,
    paths: {
      #TODO: we should make a tool or a flag to help construct those exclude lists
      # or include something like 'bento archive' in semgrep
      # or automatically add the (* nosemgrep *) annotation everywhere.
      exclude: [
	#TODO: those files contain less than 100 findings in total, so
	# they should not be too hard to fix
	# 1 finding per file
        "semgrep-core/src/analyzing/CFG_build.ml",
        "semgrep-core/src/cli-bridge/Semgrep_bridge_core.ml",
        "semgrep-core/src/core/Metavariable.ml",
        "semgrep-core/src/core/Xlang.ml",
        "semgrep-core/src/core/Xpattern.ml",
        "semgrep-core/src/core/ast/AST_generic.ml",
        "semgrep-core/src/core/ast/Meta_AST.ml",
        "semgrep-core/src/engine/Match_extract_mode.ml",
        "semgrep-core/src/engine/Metavariable_pattern.ml",
        "semgrep-core/src/core/il/Display_IL.ml",
        "semgrep-core/src/engine/Xpattern_match_comby.ml",
        "semgrep-core/src/matching/Match_patterns.ml",
        "semgrep-core/src/parsing/Parse_pattern.ml",
        "semgrep-core/src/reporting/Statistics_report.ml",
        "semgrep-core/src/runner/Parse_with_caching.ml",
        "semgrep-core/src/targeting/Skip_target.ml",
        "semgrep-core/src/tainting/Taint_lval_env.ml",
	# 2 findings per file
        "semgrep-core/src/core/Semgrep_error_code.ml",
        "semgrep-core/src/core/ast/Type_generic.ml",
        "semgrep-core/src/core/il/IL_helpers.ml",
        "semgrep-core/src/engine/Entropy.ml",
        "semgrep-core/src/engine/Match_tainting_mode.ml",
        "semgrep-core/src/engine/ReDoS.ml",
        "semgrep-core/src/fixing/Autofix_printer.ml",
        "semgrep-core/src/matching/Apply_equivalences.ml",
        "semgrep-core/src/optimizing/Analyze_rule.ml",
        "semgrep-core/src/optimizing/Bloom_annotation.ml",
        "semgrep-core/src/optimizing/Caching.ml",
        "semgrep-core/src/reporting/JSON_report.ml",
	# more findings per file
        "semgrep-core/src/analyzing/Dataflow_svalue.ml",
        "semgrep-core/src/core/ast/AST_generic_helpers.ml",
        "semgrep-core/src/engine/Match_search_mode.ml",
        "semgrep-core/src/fixing/Autofix_metavar_replacement.ml",
        "semgrep-core/src/matching/Matching_generic.ml",
        "semgrep-core/src/matching/SubAST_generic.ml",
        "semgrep-core/src/optimizing/Analyze_pattern.ml",
        "semgrep-core/src/parsing/Parse_equivalences.ml",
        "semgrep-core/src/parsing/Parse_target.ml",
        "semgrep-core/src/runner/Run_semgrep.ml",
        "semgrep-core/src/tainting/Dataflow_tainting.ml",
        "semgrep-core/src/targeting/Guess_lang.ml",
        "semgrep-core/src/utils/Regexp_engine.ml",
	#TODO: we should fix those too. They account for 464 findings in total
	"Generic_vs_generic.ml",
	"Visitor_AST.ml",
	"Naming_AST.ml",
	"Pretty_print_AST.ml",
	"Constant_propagation.ml",
	"Eval_generic.ml",
	"AST_to_IL.ml",
	"Parse_rule.ml",
	"parsing/tree_sitter/*",
	"parsing/pfff/*",
	"parsing/other/*",
	"parsing/ast/*",
	"metachecking/*",
        ] + test_code_globs + less_important_code_globs,
    }
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
