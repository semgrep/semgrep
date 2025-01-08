val with_buffer_to_string : (Format.formatter -> unit) -> string

(* Make a pp function "show-compliant" (equivalent to Fmt.to_to_string) *)
val to_show : 'a Fmt.t -> 'a -> string

(* Make a show function "pp-compliant" (equivalent to Fmt.of_to_string) *)
val of_show : ('a -> string) -> 'a Fmt.t

val pp_table : string * string list -> (string * int list) list Fmt.t
(** Pretty-prints the table with the heading. The first row are strings,
    the remaining are integers. The first row is left-aligned, all others
    right-aligned.
    [pp_table ("H1", [ "H2"; "H3"]) ppf [ ("A", [ 1; 2 ]); ("B", [ 100; 20 ]) ]]

    {[
      H1  H2 H3
      ---------
      A    1  2
      B  100 20
    ]} *)

val pp_tables :
  Format.formatter ->
  string * string list * (string * int list) list ->
  string * string list * (string * int list) list ->
  unit
(** Pretty-prints two tables with headings side by side, with some spacing in between.
    Look at [pp_table] for the individual arguments. *)

(* internals now also used by Console.ml *)

(* ex: line 5 --> "|---|" ? *)
val line : int -> string

val layout_table :
  string * string list -> (string * int list) list -> string list
