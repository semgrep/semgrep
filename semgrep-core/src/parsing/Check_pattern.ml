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
    | Cpp
    | Ocaml
    | Json
    | Yaml
    | Hcl
    | Csharp
    | Kotlin
    | Lua
    | Elixir
    | R
    | Html -> true
    | Js
    | Ts
    | Vue
    | Ruby
    | Php
    | Hack
    | Bash
    | Dockerfile
    | Rust
    | Scala
    | Solidity -> false)

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
           str (Lang.to_string lang));
    k ident
  in
  if lang_has_no_dollar_ids lang then
    Visitor_AST.(
      mk_visitor { default_visitor with kident = kident_metavar } ast)

let check lang ast =
  let error s = failwith s in
  check_pattern_metavars error lang ast
