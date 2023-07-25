open Immediate_bitfield

type packed = t
type unpacked = { hidden : bool; case_insensitive : bool } [@@deriving show]
type packed_field = int

(* Boolean fields are offsets into our immediate bitfield. *)
let hidden_field : packed_field = 0
let case_insensitive_field : packed_field = 1

let pack flags =
  let open Immediate_bitfield in
  let packed = empty in
  let packed = set_bit packed hidden_field flags.hidden in
  let packed = set_bit packed case_insensitive_field flags.case_insensitive in
  packed

(* Adding individual getters because it is more often the case that we will just care about getting one particular flag as oposed to setting one flag a time. *)
let get field flags = get_bit flags field
let is_hidden = get hidden_field
let is_case_insensitive = get case_insensitive_field

let unpack flags =
  { hidden = is_hidden flags; case_insensitive = is_case_insensitive flags }

let show_packed flags = unpack flags |> show_unpacked
let equal_packed : packed -> packed -> bool = equal
let equal_unpacked f1 f2 = equal_packed (pack f1) (pack f2)
let hash_packed = hash
let hash_fold_packed = hash_fold_t
let pp_packed _fmt _flags = ()
