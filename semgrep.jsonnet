// -*- jsonnet -*-

// ----------------------------------------------------------------------------
// Registry rules!
// ----------------------------------------------------------------------------
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
      Use `Common.with_open_infile()` instead.
    |||,
    paths: {
      // TODO, we should fix those too
      exclude: ['Common.ml', 'common2.ml'],
    },
  },
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
    languages: ['ocaml'],
    severity: 'ERROR',
    message: |||
      Do not use Http_helpers outside the networking/ directory. Move the code
      in one of the networking/ modules and hide it behind a typed interface.
    |||,
    paths: {
      exclude: ['networking'],
    },
  },
];

// ----------------------------------------------------------------------------
// TCB rules
// ----------------------------------------------------------------------------

local exit_rules = import 'TCB/forbid_exit.jsonnet';

local tcb_rules =
  [
    r { paths: { exclude: ['tools/*', 'scripts/*', '*_main.ml'] } }
    for r in exit_rules.rules
  ];

// ----------------------------------------------------------------------------
// Skip and last-minute override
// ----------------------------------------------------------------------------

// this is useful to factorize exclude
//local test_code_globs = ['Unit_*.ml', 'Test_*.ml'];
//local less_important_code_globs = ['spacegrep/', 'experiments/', 'scripts/'];

local todo_skipped_for_now = [
  //TODO? what is the fix for that?
  'ocaml.lang.portability.crlf-support.broken-input-line',
  // too noisy
  'ocaml.lang.security.marshal.ocamllint-marshal',
  'ocaml.lang.security.filenameconcat.ocamllint-filenameconcat',
  'ocaml.lang.security.tempfile.ocamllint-tempfile',
  'ocaml.lang.security.hashtable-dos.ocamllint-hashtable-dos',
  'ocaml.lang.security.digest.ocamllint-digest',
  //TODO: fix those one at least
  'ocaml.lang.security.unsafe.ocamllint-unsafe',
  'ocaml.lang.security.exec.ocamllint-exec'
];

local override_messages = {
  // semgrep specific adjustments
  'ocaml.lang.best-practice.exception.bad-reraise': |||
    You should not re-raise exceptions using 'raise' because it loses track of where the
    exception was raised originally. See commons/Exception.mli for more information.
    Use `Exception.catch exn` and later `Exception.raise exn` or
    `Exception.catch_and_reraise exn` if there is no code between the moment you
    catch the exn and re-raise it.
  |||,
};

// ----------------------------------------------------------------------------
// Entry point
// ----------------------------------------------------------------------------

local all = yml.rules + semgrep_rules + ocaml_rules + tcb_rules;

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
