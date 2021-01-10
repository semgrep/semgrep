(*s: semgrep/parsing/Check_semgrep.ml *)

(*s: constant [[Check_semgrep.lang_has_no_dollar_ids]] *)
(* for these languages, we are sure that $x is an error *)
let lang_has_no_dollar_ids = Lang.(function
  | Python | Python2 | Python3
  | Java
  | Go
  | C | Cplusplus
  | OCaml
  | JSON
  | Csharp
  | Kotlin
  | Lua
    -> true
  | Javascript | Ruby | Typescript | PHP | Rust
    -> false)
(*e: constant [[Check_semgrep.lang_has_no_dollar_ids]] *)

(*s: function [[Check_semgrep.check_pattern_metavars]] *)
let check_pattern_metavars lang ast =
  let kident_metavar (k, _out) ((str, _tok) as ident) =
    if str.[0] = '$' && not (Metavars_generic.is_metavar_name str) then
      failwith (Common.spf "`%s' is neither a valid identifier in %s nor a valid meta-variable"
                  str (Lang.string_of_lang lang));
    k ident
  in
  if lang_has_no_dollar_ids lang then
    Visitor_AST.(mk_visitor ({default_visitor with kident = kident_metavar}) ast)
(*e: function [[Check_semgrep.check_pattern_metavars]] *)

(*s: function [[Check_semgrep.check_pattern]] *)
let check_pattern lang ast =
  check_pattern_metavars lang ast
(*e: function [[Check_semgrep.check_pattern]] *)

(*s: function [[Check_semgrep.parse_check_pattern]] *)
(*e: function [[Check_semgrep.parse_check_pattern]] *)
(*e: semgrep/parsing/Check_semgrep.ml *)
