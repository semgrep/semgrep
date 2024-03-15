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

let tags = Logs_.create_tags [ __MODULE__ ]

type t = {
  include_filter : Include_filter.t option;
  gitignore_filter : Gitignore.filter;
}

type builtin_semgrepignore = Empty | Semgrep_scan_legacy

(*
   TODO: Preprocess a file to expand ':include' directives before parsing it
   using gitignore rules.

   Honor them with a deprecation warning.
*)

type exclusion_mechanism = Gitignore_and_semgrepignore | Only_semgrepignore

(*
   The legacy built-in semgrepignore.

   It was copied from templates/.semgrepignore in the Python source.
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
*_test.go

# Semgrep rules folder
.semgrep

# Semgrep-action log folder
.semgrep_logs/
|}

let gitignore_files = ("gitignore", ".gitignore")
let semgrep_ignore_files = ("semgrepignore", ".semgrepignore")

let contents_of_builtin_semgrepignore = function
  | Empty -> ""
  | Semgrep_scan_legacy -> builtin_semgrepignore_for_semgrep_scan

let create ?include_patterns ?(cli_patterns = []) ~builtin_semgrepignore
    ~exclusion_mechanism ~project_root () =
  let include_filter =
    Option.map (Include_filter.create ~project_root) include_patterns
  in
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
  let gitignore_filenames =
    match exclusion_mechanism with
    | Gitignore_and_semgrepignore -> [ gitignore_files; semgrep_ignore_files ]
    | Only_semgrepignore -> [ semgrep_ignore_files ]
  in
  (* Check if there is a top level .semgrepignore. If not use builtins *)
  let semgrep_ignore_exists =
    let gitignore_cache =
      Gitignores_cache.create ~gitignore_filenames:[ semgrep_ignore_files ]
        ~project_root ()
    in
    Gitignores_cache.load gitignore_cache Ppath.root |> Option.is_some
  in
  let higher_priority_levels =
    if semgrep_ignore_exists then [ cli_level ]
    else [ builtin_level; cli_level ]
  in
  let gitignore_filter =
    Gitignore_filter.create ~higher_priority_levels ~gitignore_filenames
      ~project_root ()
  in
  { include_filter; gitignore_filter }

let select t (git_path : Ppath.t) =
  Logs.debug (fun m ->
      m ~tags "Semgrepignore.select %s" (Ppath.to_string git_path));
  let status, sel_events =
    match t.include_filter with
    | None -> (Gitignore.Not_ignored, [])
    | Some include_filter -> Include_filter.select include_filter git_path
  in
  match status with
  | Ignored -> (Gitignore.Ignored, sel_events)
  | Not_ignored ->
      Gitignore_filter.select t.gitignore_filter sel_events git_path
[@@profiling]
