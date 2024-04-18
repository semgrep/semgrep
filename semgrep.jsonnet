// ----------------------------------------------------------------------------
// Registry rules!
// ----------------------------------------------------------------------------

// Those OCaml registry rules come from
//  https://github.com/semgrep/semgrep-rules/tree/develop/ocaml
// You can see the content of this p/ocaml ruleset by going here:
//  https://semgrep.dev/c/p/ocaml
// Here is the config file for p/ocaml that ensures it contain good rules:
//  https://github.com/semgrep/semgrep-rule-packs/blob/master/helper_scripts/generate/cfg/ocaml.yaml

local ocaml = import 'p/ocaml';

//Temporary hack to not report p/ocaml findings on semgrep libs
//TODO: we should use +: instead of :, as in
//  [ r + { paths +: { exclude +: [ "libs/*", "tools/*", "languages/*" ] } }
// but this is not supported yet by ojsonnet hence the use of :
local ocaml_rules =
  [
    r { paths: { exclude: ['libs/*', 'tools/*', 'languages/*'] } }
    for r in ocaml.rules
  ];

// ----------------------------------------------------------------------------
// legacy rules written in YAML
// ----------------------------------------------------------------------------
local yml = import 'semgrep.yml';

// ----------------------------------------------------------------------------
// jsonnet rules
// ----------------------------------------------------------------------------

local semgrep_rules = [
  // new syntax!
  {
    id: 'no-open-in',
    match: { any: ['open_in_bin ...', 'open_in ...'] },
    // Same but using The old syntax:
    //  "pattern-either": [
    //    { pattern: "open_in_bin ..." },
    //    { pattern: "open_in ..." },
    //   ],
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
      It is easy to forget to close `open_in` with `close_in`.
      Use `UCommon.with_open_infile()` or `UChan.with_open_in` instead.
    |||,
    paths: {
      // TODO, we should fix those too
      exclude: ['common2.ml'],
    },
  },
  // See also TCB/forbid_network.jsonnet
  {
    id: 'no-http-outside-networking',
    match: {
      pattern: 'Http_helpers.$F ...',
      where: [
        {
          metavariable: '$F',
          regex: '^(get|post)',
        },
      ],
    },
    paths: {
      exclude: ['networking'],
    },
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
      Do not use Http_helpers outside the networking/ directory. Move the code
      in one of the networking/ modules and hide it behind a typed interface.
    |||,
  },
  {
    id: 'no-hashtbl-find-all',
    match: 'Hashtbl.find_all',
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
      `Hashtbl.find_all` is not stack-safe in OCaml < 5. Use `Hashtbl_.push`
      instead of `Hashtbl.add` and `Hashtbl_.get_stack` instead of
      `Hashtbl.find_all`.
    |||,
  },
  {
    id: 'mask-all-temp-paths',
    pattern: 'Testo.mask_temp_paths',
    fix: 'Testutil.mask_temp_paths',
    message: |||
      Semgrep applies `Unix.realpath` to some paths resulting in
      the temporary folder being rewritten to its physical path if it
      was a symlink (MacOS). Use `Testutil.mask_temp_paths` to mask all known
      temporary folder paths in Semgrep test output.
    |||,
    languages: ['ocaml'],
    severity: 'ERROR',
  },
];

// ----------------------------------------------------------------------------
// TCB rules
// ----------------------------------------------------------------------------
local tcb = import "TCB/forbid_everything.jsonnet";

// ----------------------------------------------------------------------------
// Skip and last-minute override
// ----------------------------------------------------------------------------

// TODO? filter based on metadata like filtering all rules with
// 'confidence: LOW' or with 'subcategory: audit'?
// See also:
//  - https://www.notion.so/semgrep/Rule-Severity-Cleanup-3b9774b4614c431989fd70522a118672?pvs=4
//  - https://semgrep.dev/docs/contributing/contributing-to-semgrep-rules-repository/#including-fields-required-by-security-category
//
local todo_skipped_for_now = [
  //TODO? what is the fix for that?
  'ocaml.lang.portability.crlf-support.broken-input-line',
  // too noisy
  'ocaml.lang.security.hashtable-dos.ocamllint-hashtable-dos',
];

local override_messages = {
  // Semgrep specific adjustments
  'ocaml.lang.best-practice.exception.bad-reraise': |||
    You should not re-raise exceptions using 'raise' because it loses track
    of where the exception was raised originally. See commons/Exception.mli
    for more information.
    Use `Exception.catch exn` and later `Exception.raise exn` or
    `Exception.catch_and_reraise exn` if there is no code between the moment
    you catch the exn and re-raise it.
  |||,
};

// ----------------------------------------------------------------------------
// Entry point
// ----------------------------------------------------------------------------

local all = yml.rules + semgrep_rules + ocaml_rules + tcb.rules;

{
  rules:
    [
      if std.objectHas(override_messages, r.id)
      then (r { message: override_messages[r.id] })
      else r
      for r in all
      if !std.member(todo_skipped_for_now, r.id)
    ],
}
