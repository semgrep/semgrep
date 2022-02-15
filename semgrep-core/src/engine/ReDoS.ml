(*
   Detect regexps vulnerable to denial-of-service attacks (ReDoS) by static
   analysis.
*)

let parse_regexp conf re_str =
  try Some (Pfff_lang_regexp.Parse.string ~conf re_str)
  with Parse_info.Parsing_error _ -> None

let is_vulnerable _re =
  (* TODO *)
  true

let regexp_may_explode re_str =
  (* TODO: specify correct regexp dialect *)
  let conf = Pfff_lang_regexp.Dialect.default in
  match parse_regexp conf re_str with
  | None -> None
  | Some re_ast -> Some (is_vulnerable re_ast)
