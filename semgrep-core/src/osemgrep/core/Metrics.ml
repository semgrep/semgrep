(*
   Translated from metrics.py
*)

open Printf

let _metrics_endpoint = "https://metrics.semgrep.dev"

module State = struct
  (*
     Configures metrics upload.

     ON - Metrics always sent
     OFF - Metrics never sent
     AUTO - Metrics only sent if config is pulled from the server
  *)
  type t = On | Off | Auto

  let to_string = function
    | On -> "on"
    | Off -> "off"
    | Auto -> "auto"

  (* For Cmdliner *)
  let parser = function
    | "on" -> Ok On
    | "off" -> Ok Off
    | "auto" -> Ok Auto
    | s -> Error (sprintf "unsupported value for metrics state: %s" s)

  (* For Cmdliner *)
  let printer fmt x = Format.pp_print_string fmt (to_string x)

  (* For Cmdliner *)
  let converter = Cmdliner.Arg.conv' ~docv:"STATE" (parser, printer)
end

type _sha256hash = Sha256hash of string

(* TODO: everything else from here *)
