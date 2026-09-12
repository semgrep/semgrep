(*
   Various string manipulation functions that need unit tests.
*)

(*
   Same as String.sub but shrink the requested range to a valid range
   if needed. It will not raise an exception due to an invalid range.

   Examples:
     safe_string_sub "abcd" 1 2    = "bc"  (* valid range *)
     safe_string_sub "abcd" (-1) 2 = "a"   (* partially valid range *)
     safe_string_sub "abcd" (-5) 2 = ""    (* completely invalid range *)
*)
val safe_sub : string -> int -> int -> string
