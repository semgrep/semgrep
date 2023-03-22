(* Entry point for the osemgrep standalone program.

   Translated from __main__.py
*)

open Printf

let init () = Parsing_init.init ()

let main () =
  init ();
  let exit_code = CLI.main Sys.argv |> Exit_code.to_int in
  (* TODO: remove or make debug-only *)
  if exit_code <> 0 then
    eprintf "exiting with error status %i: %s\n%!" exit_code
      (String.concat " " (Array.to_list Sys.argv));
  exit exit_code

let () = main ()
