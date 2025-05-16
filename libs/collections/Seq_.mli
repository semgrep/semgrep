(* Pretty print a sequence separated by commas:
 *
 * let pp_brackets = pp "[" "]" in
 * pp pp_elt fmt seq
 *
 * -> "[1, 2, 3]"
 *)
val pp :
  string ->
  string ->
  (Format.formatter -> 'elt -> unit) ->
  Format.formatter ->
  'elt Seq.t ->
  unit
