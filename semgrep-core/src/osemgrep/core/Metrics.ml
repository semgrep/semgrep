(*
   Translated from metrics.py
*)

let _metrics_endpoint = "https://metrics.semgrep.dev"

module State = struct
  (*
     Configures metrics upload.

     ON - Metrics always sent
     OFF - Metrics never sent
     AUTO - Metrics only sent if config is pulled from the server
  *)
  type t = On | Off | Auto

  (* For Cmdliner *)
  (* TOPORT? use lowercase_ascii before? accept ON/OFF/AUTO?
     TOPORT? Support setting via old environment variable values 0/1/true/false
  *)
  let converter = Cmdliner.Arg.enum [ ("on", On); ("off", Off); ("auto", Auto) ]
end

type _sha256hash = Sha256hash of string

(* TODO: everything else from here *)
