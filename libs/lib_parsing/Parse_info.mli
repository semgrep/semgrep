(* TODO: remove at some point *)
type t = Tok.t [@@deriving eq, show]

(* Format the location file/line/column into a string *)
val string_of_info : t -> string
val min_max_ii_by_pos : t list -> t * t
