
(* for these languages, we are sure that $x is an error *)
let lang_has_no_dollar_ids = Lang.(function
  | Python | Python2 | Python3
  | Java
  | Go
  | C
  | ML
  -> true
  | Javascript
  -> false)

let check_pattern_metavars lang ast =
  let kident_metavar (k, _out) ((str, _tok) as ident) =
    if str.[0] = '$' && not (Metavars_generic.is_metavar_name str) then
      failwith (Common.spf "`%s' is neither a valid identifier in %s nor a valid meta-variable"
                            str (Lang.string_of_lang lang));
    k ident
    in
  if lang_has_no_dollar_ids lang then
    Visitor_ast.(mk_visitor ({default_visitor with kident = kident_metavar}) ast)

let check_pattern lang ast =
  check_pattern_metavars lang ast;
  ast

let parse_check_pattern lang str =
  Parse_generic.parse_pattern lang str
  |> check_pattern lang
