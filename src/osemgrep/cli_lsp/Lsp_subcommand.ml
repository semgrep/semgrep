(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-lsp command, execute it and exit.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

let hook_pro_language_server :
    (caps -> Eio_unix.Stdenv.base -> unit) option Hook.t =
  Hook.create None

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : < caps ; .. >) (base : Eio_unix.Stdenv.base)
    (conf : Lsp_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "Starting semgrep-lsp");
  match Hook.get hook_pro_language_server with
  | Some run_pro_language_server when conf.x_eio_ls ->
      run_pro_language_server (caps :> Legacy_language_server.caps) base;
      Exit_code.ok ~__LOC__
  | None when conf.x_eio_ls ->
      Logs.err (fun m ->
          m
            "Eio-based language server is not configured--make sure you are \
             using the proprietary semgrep binary.");
      Exit_code.fatal ~__LOC__
  | _ ->
      Legacy_language_server.start (caps :> Legacy_language_server.caps);
      Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (base : Eio_unix.Stdenv.base)
    (argv : string array) : Exit_code.t =
  let conf = Lsp_CLI.parse_argv argv in
  run_conf caps base conf
