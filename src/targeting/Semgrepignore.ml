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

type t = {
  include_filter : Include_filter.t option;
  gitignore_filter : Gitignore.filter;
}

(*
   TODO: Preprocess a file to expand ':include' directives before parsing it
   using gitignore rules.

   Honor them with a deprecation warning.
*)

type exclusion_mechanism = Gitignore_and_semgrepignore | Only_semgrepignore

let create ?include_patterns ?(cli_patterns = []) ~exclusion_mechanism
    ~project_root () =
  let include_filter =
    Option.map (Include_filter.create ~project_root) include_patterns
  in
  let root_anchor = Glob.Pattern.root_pattern in
  let cli_patterns =
    List.concat_map
      (Parse_gitignore.from_string ~name:"exclude pattern from command line"
         ~kind:"exclude" ~anchor:root_anchor)
      cli_patterns
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
    | Gitignore_and_semgrepignore ->
        [ ("gitignore", ".gitignore"); ("semgrepignore", ".semgrepignore") ]
    | Only_semgrepignore -> [ ("semgrepignore", ".semgrepignore") ]
  in
  let gitignore_filter =
    Gitignore_filter.create ~higher_priority_levels:[ cli_level ]
      ~gitignore_filenames ~project_root ()
  in
  { include_filter; gitignore_filter }

let select t (git_path : Ppath.t) =
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
