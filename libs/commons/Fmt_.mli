val with_buffer_to_string : (Format.formatter -> unit) -> string

(* Make a pp function "show-compliant" (equivalent to Fmt.to_to_string) *)
val to_show : 'a Fmt.t -> 'a -> string

(* Make a show function "pp-compliant" (equivalent to Fmt.of_to_string) *)
val of_show : ('a -> string) -> 'a Fmt.t
