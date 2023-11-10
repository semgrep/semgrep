(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-lsp command, execute it and exit.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

module Io = RPC_server.MakeLSIO (struct
  type input = Lwt_io.input_channel
  type output = Lwt_io.output_channel

  let read_line = Lwt_io.read_line_opt
  let write = Lwt_io.write
  let stdin = Lwt_io.stdin
  let stdout = Lwt_io.stdout
  let flush = Lwt_io.flush_all
  let atomic = Lwt_io.atomic

  let read_exactly inc n =
    let rec read_exactly acc n =
      if n = 0 then
        let result = String.concat "" (List.rev acc) in
        Lwt.return (Some result)
      else
        let%lwt line = Lwt_io.read ~count:n inc in
        read_exactly (line :: acc) (n - String.length line)
    in
    read_exactly [] n
end)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Lsp_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  RPC_server.io_ref := (module Io);
  Logs.debug (fun m -> m "Starting semgrep-lsp");
  Lwt_platform.run (LS.start ());
  Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Lsp_CLI.parse_argv argv in
  run conf
