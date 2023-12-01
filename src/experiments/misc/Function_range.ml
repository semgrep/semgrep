open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Return the ranges of the functions in a AST_generic program *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type function_info = { name : string; range : Tok_range.t } [@@deriving show]
type ranges = function_info list [@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let ranges (prog : AST_generic.program) : ranges =
  let visitor =
    object (_self : 'self)
      inherit [_] AST_generic.iter

      method! visit_definition env def =
        match def with
        | { name = EN (Id ((s, _), _idinfo)); _ }, FuncDef _ ->
            let ii = AST_generic_helpers.ii_of_any (Def def) in
            let range = AST_generic_helpers.range_of_tokens_unsafe ii in
            Stack_.push { name = s; range } env
        | _else_ -> ()
    end
  in
  let aref = ref [] in
  visitor#visit_program aref prog;
  List.rev !aref
