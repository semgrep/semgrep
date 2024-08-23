(*
   A dedicated type for scanning roots so as to clarify the code.
*)

type t = Fpath.t [@@deriving show]

let of_fpath x = x
let to_fpath x = x
let of_string = Fpath.v
let to_string = Fpath.to_string

(*
   Type for "processed scanning roots", which are the output of Find_targets.ml.

   They are distinguished by being either directory paths or regular files.
*)

type processed = Dir of Fppath.t | File of Fpath.t [@@deriving show]

let fpath_of_processed = function
  | Dir x -> x.fpath
  | File f -> f

let compare_processed a b =
  Fpath.compare (fpath_of_processed a) (fpath_of_processed b)
