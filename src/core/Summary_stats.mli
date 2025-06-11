(** Quick statistics on files processing time. *)

type file_time = Semgrep_output_v1_j.file_time [@@deriving show]

type t = {
  count : int;
  sum : float;
  mean : float;
  m2 : float;
  very_slow : file_time list;
}
[@@deriving show]

val zero : t
val update : t -> Fpath.t -> float -> t
val combine : t -> t -> t
