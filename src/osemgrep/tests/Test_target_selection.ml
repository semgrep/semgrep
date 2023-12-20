(*
   Test target selection on git repos with osemgrep.
*)

module T = Alcotest_ext

let with_git_repo (files : Testutil_files.t list) func =
  Testutil_files.with_tempfiles_verbose files (fun path ->
      Testutil_files.with_chdir path (fun () ->
          Git_wrapper.init ();
          Git_wrapper.add [ Fpath.v "." ];
          Git_wrapper.commit "Add all the files";
          func ()))

let osemgrep_ls () =
  (* TODO: see/reuse what's being done in Test_osemgrep.ml *)
  ()

let repos : (string * Testutil_files.t list) list =
  let open Testutil_files in
  [
    ( "simple-semgrepignore",
      [
        file "a";
        file "b";
        file "c";
        File (".gitignore", "a\n");
        File (".semgrepignore", "b\n");
      ] );
    ("no-semgrepignore", [ file "a"; File (".gitignore", "a\n") ]);
  ]

let tests =
  repos
  |> List_.map (fun (repo_name, (files : Testutil_files.t list)) ->
         T.create
           ~category:[ "target selection on real git repos" ]
           ~output_kind:Stdout
           ~mask_output:
             [
               T.mask_line ~after:"Initialized empty Git repository in" ();
               T.mask_line ~after:"[main (root-commit) " ~before:"]" ();
               T.mask_pcre_pattern "/test-[a-f0-9]+";
             ]
           repo_name
           (fun () -> with_git_repo files osemgrep_ls))
