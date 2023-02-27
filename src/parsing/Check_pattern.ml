(* for these languages, we are sure that $x is an error *)
let lang_has_no_dollar_ids =
  Lang.(
    function
    | Apex
    | Python
    | Python2
    | Python3
    | Java
    | Go
    | C
    | Cpp
    | Dart
    | Julia
    | Clojure
    | Lisp
    | Scheme
    | Ocaml
    | Json
    | Jsonnet
    | Yaml
    | Terraform
    | Csharp
    | Kotlin
    | Lua
    | Elixir
    | R
    | Swift
    | Html
    | Xml ->
        true
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
    | Solidity ->
        false)

class ['self] metavar_checker =
  object (_self : 'self)
    inherit [_] AST_generic.iter_no_id_info as super

    method! visit_ident (error, lang) id =
      let str, _tok = id in
      if
        str.[0] = '$'
        && (not (Metavariable.is_metavar_name str))
        && not (Metavariable.is_metavar_ellipsis str)
      then
        error
          (Common.spf
             "`%s' is neither a valid identifier in %s nor a valid \
              meta-variable"
             str (Lang.to_string lang));
      super#visit_ident (error, lang) id
  end

let check_pattern_metavars error lang ast =
  if lang_has_no_dollar_ids lang then
    (new metavar_checker)#visit_any (error, lang) ast

let check lang ast =
  let error s = failwith s in
  check_pattern_metavars error lang ast
