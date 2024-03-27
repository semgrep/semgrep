(*
   Various utilities used for testing and are not considered an extension
   of Alcotest (e.g. because it depends on Semgrep-specific libraries).
*)

let run what f =
  UPrintf.printf "running %s...\n%!" what;
  Common.protect ~finally:(fun () -> UPrintf.printf "done with %s.\n%!" what) f

let mask_temp_paths ?depth ?replace () =
  let mask_original_path = Testo.mask_temp_paths ?depth ?replace () in
  let mask_physical_path =
    Testo.mask_temp_paths ?depth ?replace
      ~tmpdir:(UFilename.get_temp_dir_name () |> UUnix.realpath)
      ()
  in
  fun text -> text |> mask_original_path |> mask_physical_path
