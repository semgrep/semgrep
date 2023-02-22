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

type t = Gitignore_filter.t

(*
   TODO: Preprocess a file to expand ':include' directives before parsing it
   using gitignore rules.

   Honor them with a deprecation warning.
*)

let create ?includes ?(excludes = []) ~project_root () =
  if Fpath.is_rel project_root then
    invalid_arg
      ("Semgrepignore.create needs an absolute path: "
      ^ Fpath.to_string project_root);
  let root_anchor = Glob_matcher.root_pattern in
  let patterns =
    let include_selectors =
      match includes with
      | None -> []
      | Some deexclude_patterns ->
          (* --include means "exclude everything except these patterns" *)
          let exclude_any =
            Gitignore_syntax.from_string ~anchor:root_anchor "**"
          in
          let deexclude =
            List.concat_map
              (fun str ->
                Gitignore_syntax.from_string ~anchor:root_anchor ("!" ^ str))
              deexclude_patterns
          in
          exclude_any @ deexclude
    in
    let exclude_selectors =
      List.concat_map
        (Gitignore_syntax.from_string ~anchor:root_anchor)
        excludes
    in
    include_selectors @ exclude_selectors
  in
  let cli_level : Gitignore_level.t =
    {
      level_kind = "command-line includes/excludes";
      source_name = "<command line>";
      patterns;
    }
  in
  Gitignore_filter.create ~higher_priority_levels:[ cli_level ]
    ~gitignore_filenames:[ ".gitignore"; ".semgrepignore" ]
    ~project_root ()

let select t path =
  (*
     Syntactic path normalization: 'foo/../bar' becomes 'bar' even if 'foo'
     doesn't exist. This isn't what we want: ideally, 'foo/..' should result
     in an error if 'foo' doesn't exist or isn't a readable directory.
  *)
  let git_path =
    match Git_path.of_fpath path |> Git_path.normalize with
    | Ok x -> x
    | Error msg -> failwith msg
  in
  if Git_path.is_relative git_path then
    failwith
      ("Semgrepignore.select: not an absolute path: " ^ Fpath.to_string path)
  else Gitignore_filter.select t git_path
