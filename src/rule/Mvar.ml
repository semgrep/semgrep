open Common

(* Provide hash_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

(* less: could want to remember the position in the pattern of the metavar
 * for error reporting on pattern itself? so use a 'string AST_generic.wrap'?
 *)
type t = string [@@deriving show, eq, hash]

(* ex: $X, $FAIL, $VAR2, $_
 * Note that some languages such as PHP or Javascript allows '$' in identifier
 * names, so forcing metavariables to have uppercase letters at least allow
 * us to match specifically also identifiers in lower case (e.g., $foo will
 * only match the $foo identifiers in some concrete code; this is not a
 * metavariable).
 * We allow _ as a prefix to disable the unused-metavar check (we use
 * the same convention than OCaml).
 * However this conflicts with PHP superglobals, hence the special
 * cases below in is_metavar_name.
 * coupling: AST_generic.is_metavar_name
 *)
let metavar_regexp_string = "^\\(\\$[A-Z_][A-Z_0-9]*\\)$"

(*
 * Hacks abusing existing constructs to encode extra constructions.
 * One day we will have a pattern_ast.ml that mimics mostly
 * AST.ml and extends it with special sgrep constructs.
 *)
let is_metavar_name s =
  match s with
  (* ugly: we should probably pass the language to is_metavar_name, but
   * that would require to thread it through lots of functions, so for
   * now we have this special case for PHP superglobals.
   * ref: https://www.php.net/manual/en/language.variables.superglobals.php
   *)
  | "$_SERVER"
  | "$_GET"
  | "$_POST"
  | "$_FILES"
  | "$_COOKIE"
  | "$_SESSION"
  | "$_REQUEST"
  | "$_ENV"
    (* todo: there's also "$GLOBALS" but this may interface with existing rules*)
    ->
      false
  | __else__ -> s =~ metavar_regexp_string

(* $...XXX multivariadic metavariables. Note that I initially chose
 * $X... but this leads to parsing conflicts in Javascript.
 *)
let metavar_ellipsis_regexp_string = "^\\(\\$\\.\\.\\.[A-Z_][A-Z_0-9]*\\)$"
let is_metavar_ellipsis s = s =~ metavar_ellipsis_regexp_string

let mvars_of_regexp_string s =
  Pcre2_.pcre_compile s |> Pcre2_.pcre_regexp |> Pcre2.names |> Array.to_list
  |> Common.(List_.map (fun s -> spf "$%s" s))

let is_anonymous_metavar s = s =*= "$_"

(* TODO: remove when we kill numeric capture groups *)
let metavar_for_capture_group = "^\\(\\$[0-9]+\\)$"
let is_metavar_for_capture_group s = s =~ metavar_for_capture_group
