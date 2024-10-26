(*
   Parse and interpret '.semgrepignore' files.

   Original implementation: ignores.py

   The legacy semgrepignore behavior is irregular and we're trying to move
   away from it and align closely with gitignore behavior.

   Old behavior:
   Here are the differences with gitignore listed in the legacy documentation:
   - '!' pattern negations aren't supported
   - character range patterns aren't supported
   - ':include' directives are a semgrepignore addition
   - '.semgrepignore' files placed anywhere in the tree are ignored (?)

   New behavior:
   - support '!' and character ranges to conform to gitignore syntax
   - don't support ':include' to conform to gitignore syntax
   - automatically use any '.gitignore' and use '.semgrepignore' additionally
     as if the latter was appended to the former.
   Support for negated patterns ('!') allows a .semgrepignore to
   undo exclusions made in a .gitignore.

   Migration plan:
   - print a deprecation notice if an ':include' directive is found in the
     root .semgrepignore.
   - stay silent otherwise: this is problematic only in the case of a
     .semgrepignore that doesn't include the .gitignore explicitly and
     contains fewer exclusions that the .gitignore. The new behavior will
     exclude more files than before.

   Questions:
   - Do some people really run semgrep on purpose on files that are excluded
     from source control?
     (we need to answer this to figure out the consequences of the migration
     plan)
*)
type builtin_semgrepignore = Empty | Semgrep_scan_legacy

(*
   TODO: Preprocess a file to expand ':include' directives before parsing it
   using gitignore rules.

   Honor them with a deprecation warning.
*)
type exclusion_mechanism = {
  use_gitignore_files : bool;
  use_semgrepignore_files : bool;
}

(*
   The legacy built-in semgrepignore.

   It was copied from templates/.semgrepignore in the Python source.

   Coupling:
   If you modify this file, also modify:
   OSS/cli/src/semgrep/templates/.semgrepignore
*)
let builtin_semgrepignore_for_semgrep_scan =
  {|
# Git administrative folder or file
.git

# Common large paths
node_modules/
build/
dist/
vendor/
.env/
.venv/
.tox/
*.min.js
.npm/
.yarn/

# Common test paths
test/
tests/
testsuite/
*_test.go

# Semgrep rules folder
.semgrep

# Semgrep-action log folder
.semgrep_logs/
|}

let gitignore_files = ("gitignore", ".gitignore")
let semgrepignore_files = ("semgrepignore", ".semgrepignore")

let contents_of_builtin_semgrepignore = function
  | Empty -> ""
  | Semgrep_scan_legacy -> builtin_semgrepignore_for_semgrep_scan

let create ?(cli_patterns = []) ~builtin_semgrepignore ~exclusion_mechanism
    ~project_root () =
  let root_anchor = Glob.Pattern.root_pattern in
  let builtin_patterns =
    Parse_gitignore.from_string ~name:"built-in semgrepignore patterns"
      ~source_kind:"built-in" ~anchor:root_anchor
      (contents_of_builtin_semgrepignore builtin_semgrepignore)
  in
  let cli_patterns =
    List.concat_map
      (Parse_gitignore.from_string ~name:"exclude pattern from command line"
         ~source_kind:"exclude" ~anchor:root_anchor)
      cli_patterns
  in
  let builtin_level : Gitignore.level =
    {
      level_kind = "built-in semgrepignore patterns";
      source_name = "<built-in>";
      patterns = builtin_patterns;
    }
  in
  let cli_level : Gitignore.level =
    {
      level_kind = "command-line includes/excludes";
      source_name = "<command line>";
      patterns = cli_patterns;
    }
  in
  let kinds_of_ignore_files_to_consult =
    (* order matters: first gitignore then semgrepignore *)
    (if exclusion_mechanism.use_gitignore_files then [ gitignore_files ] else [])
    @
    if exclusion_mechanism.use_semgrepignore_files then [ semgrepignore_files ]
    else []
  in
  (*
     Check if there is a top-level '.semgrepignore'. If not, use builtins.

     We don't check for '.semgrepignore' down the tree, so if a user needs
     to override the default semgrepignore rules, they need at least an
     empty root '.semgrepignore' file.
  *)
  let root_semgrepignore_exists =
    let gitignore_cache =
      Gitignore_cache.create ~gitignore_filenames:[ semgrepignore_files ]
        ~project_root ()
    in
    Gitignore_cache.load gitignore_cache Ppath.root |> Option.is_some
  in

  (*
     This condition determines whether the built-in semgrepignore rules
     should apply.
  *)
  let root_semgrepignore_exists_and_is_consulted =
    root_semgrepignore_exists && exclusion_mechanism.use_semgrepignore_files
  in

  let higher_priority_levels =
    if root_semgrepignore_exists_and_is_consulted then [ cli_level ]
    else
      (* use the built-in semgrepignore rules in the absence of a root
         '.semgrepignore' file *)
      [ builtin_level; cli_level ]
  in
  let gitignore_filter =
    Gitignore_filter.create ~higher_priority_levels
      ~gitignore_filenames:kinds_of_ignore_files_to_consult ~project_root ()
  in
  gitignore_filter
