(* Entry point for the osemgrep standalone program.

   Translated from __main__.py
*)

let register_stdlib_exception_printers () =
  (* Needs to take place after JaneStreet Base does its own registration.
     https://github.com/janestreet/base/issues/146 *)
  Printexc.register_printer (function
    | Failure msg ->
        (* Avoid unnecessary quoting of the error message *)
        Some ("Failure: " ^ msg)
    | __ -> None)

let init () =
  register_stdlib_exception_printers ();
  ()

let main () =
  init ();
  let exit_code = CLI.main Sys.argv |> Exit_code.to_int in
  (* TODO: remove or make debug-only *)
  if exit_code <> 0 then
    Printf.eprintf "exiting with error status %i: %s\n%!" exit_code
      (String.concat " " (Array.to_list Sys.argv));
  exit exit_code

let () = main ()
