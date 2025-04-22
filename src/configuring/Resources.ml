(*
   Gather and show system resources.
*)

open Common

type t = {
  num_jobs : int; (* TODO: available memory, details on the host platform, ... *)
}
[@@deriving yojson]

let resources = { num_jobs = Num_jobs.recommend_number_of_parallel_jobs () }
let show () = spf "default number of parallel jobs: %i" resources.num_jobs
let to_json () = Yojson.Safe.pretty_to_string (to_yojson resources)
