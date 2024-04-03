(* Evaluate the contents of string literals *)

(*
   Assume:
   \\ -> \
   \' -> '
   \" -> "
*)
let approximate_unescape =
  let rex = Pcre2_.regexp "\\\\[\\\\'\"]" in
  fun s ->
    Pcre2_.substitute ~rex
      ~subst:(fun s ->
        assert (String.length s = 2);
        String.sub s 1 1)
      s
