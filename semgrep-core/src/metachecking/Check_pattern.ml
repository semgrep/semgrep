(* for these languages, we are sure that $x is an error *)
let lang_has_no_dollar_ids =
  Lang.(
    function
    | Python
    | Python2
    | Python3
    | Java
    | Go
    | C
    | Cplusplus
    | OCaml
    | JSON
    | Yaml
    | HCL
    | Csharp
    | Kotlin
    | Lua
    | R
    | HTML ->
        true
    | Javascript
    | Typescript
    | Vue
    | Ruby
    | PHP
    | Hack
    | Bash
    | Rust
    | Scala ->
        false)

let check_pattern_metavars error lang ast =
  let kident_metavar (k, _out) ((str, _tok) as ident) =
    if
      str.[0] = '$'
      && (not (Metavariable.is_metavar_name str))
      && not (Metavariable.is_metavar_ellipsis str)
    then
      error
        (Common.spf
           "`%s' is neither a valid identifier in %s nor a valid meta-variable"
           str (Lang.string_of_lang lang));
    k ident
  in
  if lang_has_no_dollar_ids lang then
    Visitor_AST.(
      mk_visitor { default_visitor with kident = kident_metavar } ast)

let check lang ast =
  let error s = failwith s in
  check_pattern_metavars error lang ast
