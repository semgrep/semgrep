(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-lsp command, execute it and exit.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps = Session.caps

(* Set IO here since it is specific to the LS entrypoint *)
(* This one utilizes unix IO, but the JS version does not *)
module Io : RPC_server.LSIO = struct
  module RPC_IO =
    Lsp.Io.Make
      (struct
        include Lwt

        module O = struct
          let ( let* ) x f = Lwt.bind x f
          let ( let+ ) x f = Lwt.map f x
        end

        let raise exn = Lwt.fail exn
      end)
      (struct
        type input = Lwt_io.input_channel
        type output = Lwt_io.output_channel

        let read_line = Lwt_io.read_line_opt
        let write = Lwt_io.write

        (* LWT doesn't implement this in a nice way *)
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

  let read () = RPC_IO.read Lwt_io.stdin

  let write packet =
    Lwt_io.atomic (fun oc -> RPC_IO.write oc packet) Lwt_io.stdout

  let flush () = Lwt_io.flush Lwt_io.stdout
end

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : < caps ; .. >) (conf : Lsp_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  RPC_server.io_ref := (module Io);
  Logs.debug (fun m -> m "Starting semgrep-lsp");
  Lwt_platform.run (LS.start caps);
  Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Lsp_CLI.parse_argv argv in
  run_conf caps conf
