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
open Testutil_files (* file/dir/symlink *)
module F = Testutil_files
module Out = Semgrep_output_v1_t

(*
   TODO: create two test environments:
   - git repo
   - not a git repo
*)

(*
   git init + add all the files that we put in,
   honoring .gitignore if present.

   TODO: move this functionality to Testutil_files?
*)
let create_git_repo () =
  flush stdout;
  flush stderr;
  Git_wrapper.init ();
  Git_wrapper.add [ Fpath.v "." ];
  Git_wrapper.commit "Add files"

(*
   Generic function that puts files into a temporary workspace and lists them.

   with_git: make this a git repository
   non_git_files: extra files that must be created but won't be git-added
                  (only relevant if with_git is true)
*)
let test_find_targets ?includes ?(excludes = [])
    ?(non_git_files : F.t list = []) ~with_git name (files : F.t list) =
  let category = if with_git then "with git" else "without git" in
  let test_func () =
    printf "Test name: %s > %s\n" category name;
    F.with_tempdir ~chdir:true (fun root ->
        let git_files, non_git_files =
          if with_git then (F.sort files, F.sort non_git_files)
          else ([], F.sort (files @ non_git_files))
        in
        (match git_files with
        | [] -> ()
        | _ ->
            printf "--- Files added before 'git add' ---\n";
            print_files git_files);
        (match non_git_files with
        | [] -> ()
        | _ ->
            printf "--- Files not added to git ---\n";
            print_files non_git_files);

        F.write root git_files;
        if with_git then create_git_repo ();
        F.write root non_git_files;

        let conf =
          {
            Find_targets.default_conf with
            include_ = includes;
            exclude = excludes;
          }
        in
        let targets, skipped_targets =
          Find_targets.get_target_fpaths conf [ Scanning_root.of_fpath root ]
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
        printf "--- Selected targets ---\n";
        targets |> List.iter (fun path -> printf "selected %s\n" !!path);
        printf "--- Skipped targets ---\n";
        skipped_targets
        |> List.iter (fun (x : Out.skipped_target) ->
               printf "ignored %s [%s]\n" !!(x.path)
                 (Out.show_skip_reason x.reason)))
  in
  Testo.create name test_func ~category:[ category ] ~checked_output:Stdout
    ~normalize:
      [
        Testo.mask_temp_paths ();
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
    (* Select 'a' and 'c', not 'b'. *)
    test_find_targets ~with_git "gitignore file is always consulted"
      ~non_git_files:[ F.file "a"; F.file "b" ]
      [ F.File (".gitignore", "b\n"); F.file "c" ];
    (*
       Test that the '--include' filter takes place after all the other
       filters.
    *)
    (* Can't select file 'a' via --include when gitignoring its folder. *)
    test_find_targets ~with_git ~includes:[ "a" ]
      "gitignore file takes precedence over --include"
      [
        F.File (".gitignore", "dir\n");
        F.dir "dir" [ F.file "a"; F.file "b" ];
        F.file "c";
      ];
    (* Can't select file 'a' via --include when semgrepignoring its folder. *)
    test_find_targets ~with_git ~includes:[ "*.c" ]
      "semgrepignore file takes precedence over --include"
      [
        F.File (".semgrepignore", "dir\n");
        F.dir "dir" [ F.file "a.c"; F.file "b.c" ];
        F.file "c.c";
      ];
  ]

let tests =
  Testo.categorize "Find_targets"
    (tests_with_or_without_git ~with_git:true
    @ tests_with_or_without_git ~with_git:false)
