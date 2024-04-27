(*
   Various utilities used for testing and are not considered an extension
   of Alcotest (e.g. because it depends on Semgrep-specific libraries).
*)

let run what f =
  UPrintf.printf "running %s...\n%!" what;
  Common.protect ~finally:(fun () -> UPrintf.printf "done with %s.\n%!" what) f

(*
   Semgrep applies 'Unix.realpath' to some paths resulting in
   the temporary folder being rewritten to its physical path if it
   was a symlink (MacOS). Here, we mask all known temporary folder paths
   in Semgrep test output.

   TODO: move this to Testo once it's ok with requiring ocaml >= 4.13
   (needed for Unix.realpath)
*)
let mask_temp_paths ?depth ?replace () =
  let mask_original_path = Testo.mask_temp_paths ?depth ?replace () in
  let mask_physical_path =
    Testo.mask_temp_paths ?depth ?replace
      ~temp_dir:(UFilename.get_temp_dir_name () |> UUnix.realpath |> Fpath.v)
      ()
  in
  fun text -> text |> mask_original_path |> mask_physical_path
