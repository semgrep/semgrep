(* Evaluate the contents of string literals *)

(*
   Assume:
   \\ -> \
   \' -> '
   \" -> "
*)
let approximate_unescape =
  let rex = SPcre.regexp "\\\\[\\\\'\"]" in
  fun s ->
    SPcre.substitute ~rex
      ~subst:(fun s ->
        assert (String.length s = 2);
        String.sub s 1 1)
      s
