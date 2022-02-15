(*
   A special built-in string analyzer that estimates whether a string
   looks like a secret.
*)

(* Minimum number of bits of information in the string to consider it
   high-entropy *)
let default_min_entropy = 12

let has_high_entropy ?(min_entropy = default_min_entropy) s =
  (* a dummy condition for testing the integration *)
  String.length s >= min_entropy
