(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
   respect_gitignore: the usual meaning of this option as in targeting_conf
   i.e. whether to enable or disable Gitignore filtering when listing the
   files.
*)
let test_find_targets ?expected_outcome ?includes ?(excludes = [])
    ?(non_git_files : F.t list = []) ?(respect_gitignore = true)
    ?(exclude_binary_files = true)
    ?(extra_gitignore_patterns_to_exclude_git_untracked_files = [])
    ?(scanning_root = ".") ~with_git name (files : F.t list) =
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

    Testutil_git.with_git_repo ~verbose:true ~force_add_gitignored_files:false
      ~really_create_git_repo:with_git git_files (fun root ->
        F.write_dir root non_git_files;

        let conf =
          {
            Find_targets.default_conf with
            include_ = includes;
            exclude = excludes;
            respect_gitignore;
            exclude_binary_files;
            extra_gitignore_patterns_to_exclude_git_untracked_files;
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
            printf "%s\n" (Core_error.show err));
        printf "--- Selected targets ---\n";
        targets |> List.iter (fun path -> printf "selected %s\n" !!path);
        printf "--- Skipped targets ---\n";
        skipped_targets
        |> List.iter (fun (x : Out.skipped_target) ->
            printf "ignored %s [%s]\n" !!(x.path)
              (Out.show_skip_reason x.reason)))
  in
  Testo.create ?skipped:Testutil.skip_on_windows name test_func
    ~category:[ category ] ?expected_outcome ~checked_output:(Testo.stdout ())
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
    (* A binary file (.png extension + PNG magic bytes) is skipped with the
       'Binary' reason, while the regular source file is selected. *)
    test_find_targets ~with_git "binary file is skipped"
      [
        F.File ("logo.png", "\x89PNG\r\n\x1a\n\x00\x00\x00binary data");
        F.file "a.py";
      ];
    (* With binary filtering disabled, the same binary file is selected. *)
    test_find_targets ~with_git ~exclude_binary_files:false
      "binary file is kept when binary filtering is disabled"
      [
        F.File ("logo.png", "\x89PNG\r\n\x1a\n\x00\x00\x00binary data");
        F.file "a.py";
      ];
    (* A text file starting with magic-like bytes is NOT treated as
        binary (it's selected). *)
    test_find_targets ~with_git ".txt file with magic bytes is kept"
      [ F.File ("notes.txt", "%PDF- this is just text\n") ];
    (* A png file starting without magic bytes is NOT treated as
        binary (it's selected). *)
    test_find_targets ~with_git ".png file without magic bytes is kept"
      [ F.File ("notes.png", "this is just text") ];
    (* An extension-less ELF/Mach-O binary (e.g. a compiled executable) is
       skipped: with no extension it falls into File_type.Other and is caught
       by the magic-byte sniff. This exercises the fallback end-to-end, not
       just Skip_target.is_binary in isolation. *)
    test_find_targets ~with_git "extension-less binary is skipped"
      [
        F.File ("mytool", "\x7fELF\x02\x01\x01\x00 compiled binary");
        F.file "a.py";
      ];
  ]

(*
   '.gitignore' files are consulted only in git projects except
   for the special kind of projects 'Gitignore_project' which is used
   only in some tests.
*)
let tests_with_git_only () =
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
    test_find_targets ~with_git "respect gitignore"
      [ F.File (".gitignore", "package-lock.json\n"); F.file "package.json" ]
      ~non_git_files:[ F.file "package-lock.json" ]
      ~respect_gitignore:true;
    test_find_targets ~with_git "disable gitignore"
      [ F.File (".gitignore", "package-lock.json\n"); F.file "package.json" ]
      ~non_git_files:[ F.file "package-lock.json" ]
      ~respect_gitignore:false;
    test_find_targets ~with_git "disable gitignore with exclude"
      [
        F.File (".gitignore", "package-lock.json\ngarbage\n");
        F.dir "src" [ F.file "package.json" ];
      ]
      ~non_git_files:
        [
          F.dir "src"
            [ F.file "package-lock.json"; F.file "uv.lock"; F.file "garbage" ];
        ]
      ~respect_gitignore:false
      ~extra_gitignore_patterns_to_exclude_git_untracked_files:
        [
          (* Clever hack to exclude all regular files but still visit
           all folders *)
          "*";
          "!*/";
          (* Here start the de-exclusions of files *)
          "!package-lock.json";
          "!*.lock";
        ];
  ]

let tests =
  Testo.categorize "Find_targets"
    (tests_with_or_without_git ~with_git:true
    @ tests_with_git_only ()
    @ tests_with_or_without_git ~with_git:false)
