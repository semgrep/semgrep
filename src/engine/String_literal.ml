(* Evaluate the contents of string literals *)

(*
   Assume:
   \\ -> \
   \' -> '
   \" -> "
*)
let approximate_unescape =
  let rex = Regex.regexp "\\\\[\\\\'\"]" in
  fun s ->
    Regex.substitute ~rex
      ~subst:(fun s ->
        assert (String.length s = 2);
        String.sub s 1 1)
      s
