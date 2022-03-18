(* Evaluate string literals *)

(*
   Assume:
   \\ -> \
   \' -> '
   \" -> "
*)
let unescape =
  let rex = SPcre.regexp "\\\\[\\\\'\"]" in
  fun s ->
    Pcre.substitute ~rex
      ~subst:(fun s ->
        assert (String.length s = 2);
        String.sub s 1 1)
      s

(*
   HACK!
   Remove the quotes from string literals because string literals are
   what we're getting. The generic AST should offer a view into
   string content rather than the original quoted and escaped literals.
*)
let evaluate s =
  let len = String.length s in
  if len < 2 then s
  else
    let first = s.[0] in
    let last = s.[len - 1] in
    match (first, last) with
    | '\'', '\''
    | '"', '"' ->
        String.sub s 1 (len - 2) |> unescape
    | _ -> s
