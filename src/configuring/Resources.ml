(*
   Gather and show system resources.
*)

open Common

(* TODO: available memory, details on the host platform, ... *)
type t = { cpu : Num_jobs.t } [@@deriving yojson]

let resources = { cpu = Num_jobs.get () }

let show () =
  spf {|host CPUs: %i
available CPUs: %i
default number of parallel jobs: %i|}
    resources.cpu.host_cpus resources.cpu.available_cpus
    resources.cpu.recommended_parmap_jobs

let to_json () = Yojson.Safe.pretty_to_string (to_yojson resources)
