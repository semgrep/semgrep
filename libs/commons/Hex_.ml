(*
   Encode and decode strings to hexadecimal.

   Wrapper around the Hex library (whose interface is weird).

   We assume the Hex library enforces a consistent encoding for a given
   string (a = b <=> hex a = hex b).
*)

(* A guaranteed lowercase hex representation of a string *)
type t = string

let equal = String.equal
let compare = String.compare
let show x = x
let pp fmt x = Format.pp_print_string fmt x

(* Re-encode:
   - validate
   - normalize
*)
let of_hex_string_opt orig_hex_data =
  try
    let decoded = Hex.to_string (`Hex orig_hex_data) in
    let (`Hex encoded) = Hex.of_string decoded in
    Some encoded
  with
  | _ -> None

(* Turn the private type into a string (see mli) *)
let to_hex_string x = x

let encode data =
  let (`Hex encoded) = Hex.of_string data in
  encoded

(* This won't fail because the module interface guarantees that hex_data
   is well-formed. *)
let decode hex_data = Hex.to_string (`Hex hex_data)
