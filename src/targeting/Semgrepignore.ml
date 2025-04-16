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
   - ':include' is still supported in '.semgrepignore' files but
     not recursively (included files must obey strict Gitignore syntax).

   Support for negated patterns ('!') allows a .semgrepignore to
   undo exclusions made in an included '.gitignore'.

   Note that at some point before v2 became the default,
   any '.gitignore' files would be loaded in Git project just like
   '.semgrepignore' files. While this allowed '.semgrepignore' files
   to follow the Gitignore syntax strictly, it was causing Git-tracked
   files to be Semgrepignored if they were Gitignored, causing a new
   behavior for users migrating from v1 to v2. Instead, we decided to keep
   the ':include' extension.
*)
open Fpath_.Operators

type default_semgrepignore_patterns = Empty | Semgrep_scan_legacy
type exclusion_mechanism = { use_semgrepignore_files : bool }

(*
   The default semgrepignore used when no .semgrepignore exists
   at the project root (osemgrep) or in the current folder (legacy pysemgrep).

   It was copied from templates/.semgrepignore in the Python source.

   Coupling:
   If you modify this file, also modify:
   OSS/cli/src/semgrep/templates/.semgrepignore
*)
let default_semgrepignore_for_semgrep_scan =
  {|
# Git administrative folder or file
.git
.svn
.hg
_darcs
CVS

# Common large paths
build/
vendor/
dist/
*.min.js
.env/
.tox/

# package managers
node_modules/
.npm/
.yarn/
.venv/
_opam/
_build/
_cargo/
# note that PHP composer uses vendor/ and C++ conan uses build/
# .venv is used both by Go and Python

# Common test paths
test/
tests/
testsuite/
*_test.go
|}

let semgrepignore_files : Gitignore.gitignore_filename =
  {
    source_kind = "semgrepignore";
    filename = ".semgrepignore";
    format = Gitignore.Legacy_semgrepignore;
  }

let contents_of_builtin_semgrepignore = function
  | Empty -> ""
  | Semgrep_scan_legacy -> default_semgrepignore_for_semgrep_scan

let create ?(cli_patterns = []) ~default_semgrepignore_patterns
    ~exclusion_mechanism ~project_root () =
  let root_anchor = Glob.Pattern.root_pattern in
  let default_patterns =
    Parse_gitignore.from_string ~name:"default semgrepignore patterns"
      ~source_kind:"default" ~anchor:root_anchor
      (contents_of_builtin_semgrepignore default_semgrepignore_patterns)
  in
  let cli_patterns =
    List.concat_map
      (Parse_gitignore.from_string ~name:"exclude pattern from command line"
         ~source_kind:"exclude" ~anchor:root_anchor)
      cli_patterns
  in
  let default_semgrepignore_file_level : Gitignore.level =
    {
      level_kind = "default semgrepignore patterns";
      source_name = "<built-in>";
      patterns = default_patterns;
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
    (* We used to load '.gitignore' files as well here but this plan was
       abandoned. *)
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
    let root_dir = Ppath.to_fpath ~root:project_root Ppath.root in
    let semgrepignore_path = root_dir / ".semgrepignore" in
    Sys_.Fpath.exists semgrepignore_path
  in

  (*
     This condition determines whether the default semgrepignore rules
     should apply.
  *)
  let use_default_semgrepignore =
    exclusion_mechanism.use_semgrepignore_files && not root_semgrepignore_exists
  in

  let higher_priority_levels =
    if use_default_semgrepignore then
      (* use the built-in semgrepignore rules in the absence of a root
         '.semgrepignore' file *)
      [ default_semgrepignore_file_level; cli_level ]
    else [ cli_level ]
  in
  let gitignore_filter =
    Gitignore_filter.create ~higher_priority_levels
      ~gitignore_filenames:kinds_of_ignore_files_to_consult ~project_root ()
  in
  gitignore_filter
