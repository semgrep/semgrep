(*
   Tests for the Find_targets module.

   Unlike the unit tests for semgrepignore/include/exclude
   (in Unit_semgrepignore), this exercises the two main ways we have to list
   target files:

   - using 'git ls-files';
   - by scanning the file system ourselves.

   They differ in that the former returns a list of regular files, while
   the latter performs filtering on folder paths.
*)

open Printf
open Fpath_.Operators
module F = Testutil_files
module Out = Semgrep_output_v1_t

(*
   TODO: create two test environments:
   - git repo
   - not a git repo
*)

(*
   Generic function that puts files into a temporary workspace and lists them.

   with_git: make this a git repository
   non_git_files: extra files that must be created but won't be git-added
                  (only relevant if with_git is true)
*)
let test_find_targets ?expected_outcome ?includes ?(excludes = [])
    ?(non_git_files : F.t list = []) ~with_git ?(scanning_root = ".") name
    (files : F.t list) =
  let category = if with_git then "with git" else "without git" in
  let test_func () =
    printf "Test name: %s > %s\n" category name;
    let git_files, non_git_files =
      if with_git then (F.sort files, F.sort non_git_files)
      else ([], F.sort (files @ non_git_files))
    in
    (match git_files with
    | [] -> ()
    | _ ->
        printf "--- Files added before 'git add' ---\n";
        F.print_files git_files);
    (match non_git_files with
    | [] -> ()
    | _ ->
        printf "--- Files not added to git ---\n";
        F.print_files non_git_files);

    Testutil_git.with_git_repo ~verbose:true ~honor_gitignore:true
      ~really_create_git_repo:with_git git_files (fun root ->
        F.write root non_git_files;

        let conf =
          {
            Find_targets.default_conf with
            include_ = includes;
            exclude = excludes;
          }
        in
        let targets, errors, skipped_targets =
          Find_targets.get_target_fpaths conf
            [ Scanning_root.of_fpath (Fpath.v scanning_root) ]
        in
        (match includes with
        | None -> ()
        | Some patterns ->
            printf "--- '--include' patterns ---\n";
            patterns |> List.iter (fun pat -> printf "%s\n" pat));
        (match excludes with
        | [] -> ()
        | patterns ->
            printf "--- '--exclude' patterns ---\n";
            patterns |> List.iter (fun pat -> printf "%s\n" pat));
        printf "--- Errors ---\n";
        errors
        |> List.iter (fun err ->
               (* showing some ugly JSON is better than nothing *)
               printf "%s\n" (Semgrep_output_v1_j.string_of_core_error err));
        printf "--- Selected targets ---\n";
        targets |> List.iter (fun path -> printf "selected %s\n" !!path);
        printf "--- Skipped targets ---\n";
        skipped_targets
        |> List.iter (fun (x : Out.skipped_target) ->
               printf "ignored %s [%s]\n" !!(x.path)
                 (Out.show_skip_reason x.reason)))
  in
  Testo.create name test_func ~category:[ category ] ?expected_outcome
    ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        Testutil.mask_temp_paths ();
        Testo.mask_line ~after:"(root-commit) " ~before:"]" ();
      ]

let tests_with_or_without_git ~with_git =
  [
    test_find_targets ~with_git "basic test" [ F.File (".gitignore", "") ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git "basic gitignore"
      [ F.File (".gitignore", "b\n"); F.file "a"; F.file "b" ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git "basic semgrepignore"
      [ F.File (".semgrepignore", "b\n"); F.file "a"; F.file "b" ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git ~excludes:[ "b" ] "basic exclude"
      [ F.file "a"; F.file "b" ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git ~includes:[ "a" ] "basic include"
      [ F.file "a"; F.file "b" ];
    (* Select file 'a', not 'b' *)
    test_find_targets ~with_git ~includes:[ "a" ] "deep include"
      [ F.dir "dir" [ F.file "a"; F.file "b" ] ];
    test_find_targets ~with_git ~scanning_root:"a.py" "scanning root as a file"
      [ F.file "a.py" ];
    (* Select the symlink and not the regular file it's pointing to. *)
    test_find_targets ~with_git ~scanning_root:"a.py"
      "scanning root as a symlink to a regular file"
      [ F.Symlink ("a.py", "b.py"); F.File ("b.py", "some content") ];
    test_find_targets ~with_git ~scanning_root:"a.py"
      "scanning root as a symlink to a missing regular file"
      [ F.Symlink ("a.py", "b.py") ];
    test_find_targets ~with_git ~scanning_root:"link-to-src"
      "scanning root as a symlink to a folder"
      [ F.dir "src" [ F.file "a.py" ]; F.Symlink ("link-to-src", "src") ];
    (*
       Test that the '--include' filter takes place after all the other
       filters.
    *)
    (* Can't select file 'a' via --include when semgrepignoring its folder. *)
    test_find_targets ~with_git ~includes:[ "*.c" ]
      "semgrepignore file takes precedence over --include"
      [
        F.File (".semgrepignore", "dir\n");
        F.dir "dir" [ F.file "a.c"; F.file "b.c" ];
        F.file "c.c";
      ];
    (* An explicit target is a scanning root that's also a target file
       and should not be ignored by the usual exclusion mechanisms
       (.semgrepignore, --include, --exclude) *)
    test_find_targets ~with_git ~scanning_root:"a.py" "scan explicit target"
      [ F.file "a.py"; F.File (".semgrepignore", "a.py\n") ];
    (* Unspecified behavior: what to do with a scanning root that's
       a symlink to a file that's semgrepignored? Should it be considered
       an explicit target? This test assumes so. We could change it. *)
    test_find_targets ~with_git ~scanning_root:"symlink.py"
      "scan symlink to semgrepignored target"
      [
        F.symlink "symlink.py" "semgrepignored.py";
        F.file "semgrepignored.py";
        F.File (".semgrepignore", "semgrepignored.py\n");
      ];
  ]

(*
   '.gitignore' files are consulted only in git projects except
   for the special kind of projects 'Gitignore_project' which is used
   only in some tests.
*)
let tests_with_git_only =
  let with_git = true in
  [
    (* Select 'a' and 'c', not 'b'. *)
    test_find_targets ~with_git "gitignore file is always consulted"
      ~non_git_files:[ F.file "a"; F.file "b" ]
      [ F.File (".gitignore", "b\n"); F.file "c" ];
    (* Can't select file 'a' via --include when gitignoring its folder. *)
    test_find_targets ~with_git ~includes:[ "a" ]
      "gitignore file takes precedence over --include"
      [
        F.File (".gitignore", "dir\n");
        F.dir "dir" [ F.file "a"; F.file "b" ];
        F.file "c";
      ];
    test_find_targets ~with_git "symlinks from git are filtered too"
      [ F.Symlink ("lnk", "missing"); F.File ("a", "some content") ];
  ]

let tests =
  Testo.categorize "Find_targets"
    (tests_with_or_without_git ~with_git:true
    @ tests_with_git_only
    @ tests_with_or_without_git ~with_git:false)
