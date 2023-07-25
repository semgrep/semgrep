type packed
type unpacked = { hidden : bool; case_insensitive : bool }

val pack : unpacked -> packed
val unpack : packed -> unpacked
val is_hidden : packed -> bool
val is_case_insensitive : packed -> bool
val show_packed : packed -> string
val show_unpacked : unpacked -> string
val equal_packed : packed -> packed -> bool
val equal_unpacked : unpacked -> unpacked -> bool
val hash_packed : packed -> Base.Hash.hash_value
val pp_packed : packed Fmt.t
val hash_fold_packed : Base.Hash.state -> packed -> Base.Hash.state
