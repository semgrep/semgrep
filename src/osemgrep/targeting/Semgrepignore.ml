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
  if Fpath.is_relative project_root then
    invalid_arg ("Semgrepignore.create needs an absolute path: "
                 ^ Fpath.to_string project_root);
  let root_anchor = Glob_matcher.root_pattern in
  let patterns =
    let include_selectors =
      match includes with
      | None -> []
      | Some deexclude_patterns ->
          (* --include means "exclude everything except these patterns" *)
          let exclude_any =
            Gitignore_syntax.from_string ~anchor:root_anchor "**" in
          let deexclude =
            Common.map (fun str ->
              Gitignore_syntax.from_string ~anchor:root_anchor ("!" ^ str)
            ) string_patterns
          in
          exclude_any :: deexclude
    in
    let exclude_selectors =
      Common.map (Gitignore_syntax.from_string ~anchor:root_anchor) excludes
    in
    include_selectors @ exclude_selectors
  in
  let cli_level = {
    level_kind = "command-line includes/excludes";
    source_name = "<command line>";
    patterns;
  }
  in
  Gitignore_filter.create
    ~higher_priority_levels:[cli_level]
    ~gitignore_filenames:[".gitignore"; ".semgrepignore"]
    ~project_root
    ()
(*
(* How should we determine whether a relative path doesn't point outside
   the git project? *)
let normalize_path project_root path =
  ...

let select t path =
  match Fpath.(v path |> segs) with
  | [""] :: _ -> ()
  |

  Gitignore_filter.select t git_path
*)
