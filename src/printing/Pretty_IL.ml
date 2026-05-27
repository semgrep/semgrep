(* Copyright (C) 2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
module G = AST_generic

(* NOTE: Mostly auto-generated. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let spf = Printf.sprintf

let truncate max_len str =
  if String.length str > max_len then Str.first_chars str max_len ^ "..."
  else str

module H = AST_generic_helpers

type orig_info = { desc : string option; range : (Loc.t * Loc.t) option }

(* Get a short description and location range from an IL orig. *)
let orig_info = function
  | IL.NoOrig -> { desc = None; range = None }
  | IL.SameAs e ->
      let desc =
        match e.G.e with
        | G.Call _ -> Some "Call"
        | G.Assign _ -> Some "Assign"
        | G.AssignOp _ -> Some "AssignOp"
        | G.Conditional _ -> Some "Conditional"
        | G.L lit -> Some (spf "Lit: %s" (G.show_literal lit |> truncate 20))
        | G.N _ -> Some "Name"
        | G.DotAccess _ -> Some "Dot"
        | G.ArrayAccess _ -> Some "Index"
        | _ -> Some "Expr"
      in
      { desc; range = H.range_of_any_opt (G.E e) }
  | IL.Related any ->
      let desc =
        match any with
        | G.E e -> (
            match e.G.e with
            | G.Call _ -> Some "Call"
            | _ -> Some "Expr")
        | G.S s -> (
            match s.G.s with
            | G.ExprStmt _ -> Some "ExprStmt"
            | _ -> Some "Stmt")
        | G.Tk _ -> None
        | _ -> Some "Any"
      in
      { desc; range = H.range_of_any_opt any }

(* Format origin annotation like [Call @l.42-43] *)
let pretty_orig_annot orig =
  let { desc; range } = orig_info orig in
  match desc with
  | None -> ""
  | Some desc -> (
      match range with
      | Some (start_loc, end_loc) ->
          let start_line = start_loc.Loc.pos.Pos.line in
          let end_line = end_loc.Loc.pos.Pos.line in
          if start_line = end_line then spf "  // [%s @l.%d]" desc start_line
          else spf "  // [%s @l.%d-%d]" desc start_line end_line
      | None -> spf "  // [%s]" desc)

(* Make indentation string *)
let indent_str n = String.make (n * 2) ' '

(*****************************************************************************)
(* Names and Lvalues *)
(*****************************************************************************)

let pretty_name name = IL.str_of_name name

let pretty_var_special = function
  | IL.This -> "this"
  | IL.Super -> "super"
  | IL.Self -> "self"
  | IL.Parent -> "parent"

let rec pretty_base = function
  | IL.Var name -> pretty_name name
  | IL.VarSpecial (vs, _) -> pretty_var_special vs
  | IL.Mem e -> spf "*(%s)" (pretty_exp e)

and pretty_offset_kind = function
  | IL.Dot name -> spf ".%s" (fst name.IL.ident)
  | IL.Index e -> spf "[%s]" (pretty_exp e)

and pretty_lval lval =
  let base_str = pretty_base lval.IL.base in
  let offsets_str =
    lval.IL.rev_offset
    |> List.rev_map (fun o -> pretty_offset_kind o.IL.o)
    |> String.concat ""
  in
  base_str ^ offsets_str

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

and pretty_composite_kind = function
  | IL.CTuple -> "Tuple"
  | IL.CArray -> "Array"
  | IL.CList -> "List"
  | IL.CSet -> "Set"
  | IL.Constructor name -> spf "Constructor(%s)" (fst name.IL.ident)
  | IL.Regexp -> "Regexp"

and pretty_field_or_entry = function
  | IL.Field (name, e) -> spf "%s: %s" (fst name.IL.ident) (pretty_exp e)
  | IL.Entry (k, v) -> spf "%s:-> %s" (pretty_exp k) (pretty_exp v)
  | IL.Spread e -> spf "...%s" (pretty_exp e)

and pretty_argument pretty_x = function
  | IL.Unnamed x -> pretty_x x
  | IL.Named (id, x) -> spf "%s: %s" (fst id) (pretty_x x)

and pretty_operator (op, _) =
  match op with
  | G.Plus -> "+"
  | G.Minus -> "-"
  | G.Mult -> "*"
  | G.Div -> "/"
  | G.Mod -> "%"
  | G.Pow -> "**"
  | G.FloorDiv -> "//"
  | G.MatMult -> "@"
  | G.LSL -> "<<"
  | G.LSR -> ">>>"
  | G.ASR -> ">>"
  | G.BitOr -> "|"
  | G.BitXor -> "^"
  | G.BitAnd -> "&"
  | G.BitNot -> "~"
  | G.BitClear -> "&^"
  | G.Not -> "!"
  | G.And -> "&&"
  | G.Or -> "||"
  | G.Xor -> "xor"
  | G.Eq -> "=="
  | G.NotEq -> "!="
  | G.PhysEq -> "==="
  | G.NotPhysEq -> "!=="
  | G.Lt -> "<"
  | G.LtE -> "<="
  | G.Gt -> ">"
  | G.GtE -> ">="
  | G.Cmp -> "<=>"
  | G.Concat -> "++"
  | G.Append -> "::"
  | G.RegexpMatch -> "=~"
  | G.NotMatch -> "!~"
  | G.Range -> ".."
  | G.RangeInclusive -> "..="
  | G.NotNullPostfix -> "!!"
  | G.Length -> "#"
  | G.Elvis -> "?:"
  | G.Nullish -> "??"
  | G.In -> "in"
  | G.NotIn -> "not in"
  | G.Is -> "is"
  | G.NotIs -> "is not"
  | G.Background -> "&"
  | G.Pipe -> "|>"
  | G.LDA -> "<=="
  | G.RDA -> "==>"
  | G.LSA -> "<--"
  | G.RSA -> "-->"

and pretty_literal = function
  | G.Bool (b, _) -> string_of_bool b
  | G.Int (Some i, _) -> Int64.to_string i
  | G.Int (None, _) -> "<int>"
  | G.Float (Some f, _) -> string_of_float f
  | G.Float (None, _) -> "<float>"
  | G.Char (s, _) -> spf "'%s'" s
  | G.String (_, (s, _), _) ->
      let truncated = truncate 50 s in
      spf "\"%s\"" truncated
  | G.Regexp ((_, (pattern, _), _), modifiers_opt) ->
      let truncated = truncate 50 pattern in
      let mods =
        match modifiers_opt with
        | Some (m, _) -> m
        | None -> ""
      in
      spf "/%s/%s" truncated mods
  | G.Atom _ -> ":atom"
  | G.Unit _ -> "()"
  | G.Null _ -> "null"
  | G.Undefined _ -> "undefined"
  | G.Imag _ -> "<imag>"
  | G.Ratio _ -> "<ratio>"

and pretty_exp_kind = function
  | IL.Fetch lval -> pretty_lval lval
  | IL.Literal lit -> pretty_literal lit
  | IL.Composite (kind, (_, es, _)) ->
      let kind_str = pretty_composite_kind kind in
      let es_str = es |> List.map pretty_exp |> String.concat ", " in
      spf "%s(%s)" kind_str es_str
  | IL.RecordOrDict fields ->
      let fields_str =
        fields |> List.map pretty_field_or_entry |> String.concat ", "
      in
      spf "{ %s }" fields_str
  | IL.Cast (ty, e) ->
      spf "((%s) %s)" (Pretty_print_AST.type_ ty) (pretty_exp e)
  | IL.Operator (op, args) ->
      let op_str = pretty_operator op in
      let args_str =
        args |> List.map (pretty_argument pretty_exp) |> String.concat ", "
      in
      if List.length args = 1 then spf "%s%s" op_str args_str
      else if List.length args = 2 then
        spf "(%s %s %s)"
          (List.nth args 0 |> pretty_argument pretty_exp)
          op_str
          (List.nth args 1 |> pretty_argument pretty_exp)
      else spf "%s(%s)" op_str args_str
  | IL.FixmeExp (kind, _any, exp_opt) ->
      let kind_str =
        match kind with
        | IL.ToDo -> "TODO"
        | IL.Sgrep_construct -> "SGREP"
        | IL.Impossible -> "IMPOSSIBLE"
      in
      let exp_str =
        match exp_opt with
        | Some e -> spf " partial:%s" (pretty_exp e)
        | None -> ""
      in
      spf "FIXME<%s%s>" kind_str exp_str

and pretty_exp e =
  let exp_str = pretty_exp_kind e.IL.e in
  exp_str

(*****************************************************************************)
(* Instructions *)
(*****************************************************************************)

let pretty_call_special = function
  | IL.Eval -> "eval"
  | IL.Typeof -> "typeof"
  | IL.Instanceof -> "instanceof"
  | IL.Sizeof -> "sizeof"
  | IL.Concat -> "concat"
  | IL.SpreadFn -> "spread"
  | IL.Yield -> "yield"
  | IL.Await -> "await"
  | IL.Delete -> "delete"
  | IL.Assert -> "assert"
  | IL.Ref -> "ref"
  | IL.ForeachNext -> "foreach_next"
  | IL.ForeachHasNext -> "foreach_has_next"
  | IL.Require -> "require"

let rec pretty_instr_kind ?indent:_ = function
  | IL.Assign (lv, e) -> spf "%s = %s" (pretty_lval lv) (pretty_exp e)
  | IL.AssignCall (lv_opt, { c = IL.Call (fn, args); _ }) ->
      let lv_str =
        match lv_opt with
        | Some lv -> spf "%s = " (pretty_lval lv)
        | None -> ""
      in
      let fn_str = pretty_exp fn in
      let args_str =
        args |> List.map (pretty_argument pretty_exp) |> String.concat ", "
      in
      spf "%s%s(%s)" lv_str fn_str args_str
  | IL.AssignCall (lv_opt, { c = IL.CallSpecial ((special, _), args); _ }) ->
      let lv_str =
        match lv_opt with
        | Some lv -> spf "%s = " (pretty_lval lv)
        | None -> ""
      in
      let special_str = pretty_call_special special in
      let args_str =
        args |> List.map (pretty_argument pretty_exp) |> String.concat ", "
      in
      spf "%s%s(%s)" lv_str special_str args_str
  | IL.New (lv, ty, ctor_opt, args) ->
      let ty_str = Pretty_print_AST.type_ ty in
      let resolved = Option.is_some ctor_opt in
      let args_str =
        args |> List.map (pretty_argument pretty_exp) |> String.concat ", "
      in
      spf "%s = new<resolved: %b> %s(%s)" (pretty_lval lv) resolved ty_str
        args_str
  | IL.FixmeInstr (kind, _any) ->
      let kind_str =
        match kind with
        | IL.ToDo -> "TODO"
        | IL.Sgrep_construct -> "SGREP"
        | IL.Impossible -> "IMPOSSIBLE"
      in
      spf "FIXME_INSTR<%s>" kind_str

and pretty_instr ?(indent = 0) instr =
  let instr_str = pretty_instr_kind ~indent instr.IL.i in
  let orig_str = pretty_orig_annot instr.IL.iorig in
  instr_str ^ orig_str

(*****************************************************************************)
(* Params *)
(*****************************************************************************)

and pretty_param = function
  | IL.Param { pname; pdefault } ->
      let default_str =
        match pdefault with
        | Some { dinit = []; dexp } -> spf " = %s" (pretty_exp dexp)
        | Some { dinit; dexp } ->
            spf " = {\n%s\n  %s\n}"
              (pretty_stmts ~indent:1 dinit)
              (pretty_exp dexp)
        | None -> ""
      in
      spf "%s%s" (fst pname.IL.ident) default_str
  | IL.PatternParam _ -> "<pattern>"
  | IL.ParamRest pname -> spf "...%s" (fst pname.IL.ident)
  | IL.FixmeParam -> "<fixme>"

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

and pretty_stmt ?(indent = 0) stmt =
  let ind = indent_str indent in
  let ind1 = indent_str (indent + 1) in
  match stmt.IL.s with
  | IL.Instr instr -> ind ^ pretty_instr ~indent instr ^ ";"
  | IL.If (_, cond, then_stmts, else_stmts) ->
      let cond_str = pretty_exp cond in
      let then_str = pretty_stmts ~indent:(indent + 1) then_stmts in
      let else_str =
        if else_stmts = [] then ""
        else
          "\n" ^ ind ^ "} else {\n"
          ^ pretty_stmts ~indent:(indent + 1) else_stmts
      in
      spf "%sif (%s) {\n%s\n%s}" ind cond_str then_str (ind ^ else_str ^ "}")
  | IL.Loop (_, cond, body) ->
      let cond_str = pretty_exp cond in
      let body_str = pretty_stmts ~indent:(indent + 1) body in
      spf "%swhile (%s) {\n%s\n%s}" ind cond_str body_str (ind ^ "}")
  | IL.Return (_, e) -> spf "%sreturn %s;" ind (pretty_exp e)
  | IL.Goto (_, (lbl, _)) -> spf "%sgoto %s;" ind (fst lbl)
  | IL.Label (lbl, _) -> spf "%s%s:" ind (fst lbl)
  | IL.Try (try_stmts, catches, else_stmts, finally_stmts) ->
      let try_str = pretty_stmts ~indent:(indent + 1) try_stmts in
      let catches_str =
        catches
        |> List.map (fun (name, catch_stmts) ->
            let catch_body = pretty_stmts ~indent:(indent + 1) catch_stmts in
            spf "%s} catch (%s) {\n%s" ind (fst name.IL.ident) catch_body)
        |> String.concat "\n"
      in
      let else_str =
        if else_stmts = [] then ""
        else
          "\n" ^ ind ^ "} else {\n"
          ^ pretty_stmts ~indent:(indent + 1) else_stmts
      in
      let finally_str =
        if finally_stmts = [] then ""
        else
          "\n" ^ ind ^ "} finally {\n"
          ^ pretty_stmts ~indent:(indent + 1) finally_stmts
      in
      spf "%stry {\n%s\n%s%s%s\n%s}" ind try_str catches_str else_str
        finally_str (ind ^ "}")
  | IL.Throw (_, e) -> spf "%sthrow %s;" ind (pretty_exp e)
  | IL.Match { scrutinee; branches } ->
      let scrutinee_str = fst scrutinee.IL.ident in
      let branches_str =
        branches
        |> List.map (fun { IL.pattern; body } ->
            let pat_str =
              match pattern with
              | IL.PatLiteral lit -> G.show_literal lit
              | IL.PatWildcard -> "_"
              | IL.PatVariable name -> fst name.IL.ident
              | IL.PatConstructor (name, args) ->
                  let args_str =
                    args
                    |> List.map (fun n -> fst n.IL.ident)
                    |> String.concat ", "
                  in
                  spf "%s(%s)" (fst name.IL.ident) args_str
            in
            let body_str = pretty_stmts ~indent:(indent + 2) body in
            spf "%scase %s:\n%s" ind1 pat_str body_str)
        |> String.concat "\n"
      in
      spf "%smatch (%s) {\n%s\n%s}" ind scrutinee_str branches_str ind
  | IL.NestedDef def -> pretty_definition ~indent def
  | IL.MiscStmt other -> (
      match other with
      | IL.DefStmt def -> pretty_definition ~indent def
      | IL.DirectiveStmt _ -> ind ^ "// <directive>"
      | IL.Noop msg -> ind ^ spf "// noop: %s" msg)
  | IL.FixmeStmt (kind, _) ->
      let kind_str =
        match kind with
        | IL.ToDo -> "TODO"
        | IL.Sgrep_construct -> "SGREP"
        | IL.Impossible -> "IMPOSSIBLE"
      in
      ind ^ spf "FIXME_STMT<%s>;" kind_str

and pretty_stmts ?(indent = 0) stmts =
  stmts |> List.map (pretty_stmt ~indent) |> String.concat "\n"

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

and pretty_entity_name = function
  | IL.EN name -> IL.str_of_name name
  | IL.FixmeEntity
      (G.En
         {
           name =
             G.EN (G.Id (ident, _) | G.IdQualified { name_last = ident, _; _ });
           _;
         }) ->
      spf "<fixme_entity: %s>" (fst ident)
  | IL.FixmeEntity _any -> "<fixme_entity>"

and pretty_function_definition ?(name = "") ?(indent = 0) ?(inline = false) fdef
    =
  let ind = indent_str indent in
  let params_str =
    fdef.IL.fparams |> List.map pretty_param |> String.concat ", "
  in
  let ret_str =
    match fdef.IL.frettype with
    | Some ty -> spf ": %s" (Pretty_print_AST.type_ ty)
    | None -> ""
  in
  let body_str = pretty_stmts ~indent:(indent + 1) fdef.IL.fbody in
  let header_indent = if inline then "" else ind in
  spf "%sfunction %s(%s)%s {\n%s\n%s}" header_indent name params_str ret_str
    body_str ind

and pretty_class_definition ?(name = "Class") ?(indent = 0) ?(inline = false)
    cdef =
  let ind = indent_str indent in
  let ind1 = indent_str (indent + 1) in
  let extends_str =
    match cdef.IL.cextends with
    | [] -> ""
    | parents ->
        " extends "
        ^ (parents
          |> List.map (fun (ty, _args_opt) -> Pretty_print_AST.type_ ty)
          |> String.concat ", ")
  in
  let implements_str =
    match cdef.IL.cimplements with
    | [] -> ""
    | impls ->
        " implements "
        ^ (impls
          |> List.map (fun ty -> Pretty_print_AST.type_ ty)
          |> String.concat ", ")
  in
  let fields_str =
    cdef.IL.cfields
    |> List.map (fun (ent, vdef) ->
        let field_name = pretty_entity_name ent.IL.name in
        let ty_str =
          match vdef.IL.vtype with
          | Some ty -> spf ": %s" (Pretty_print_AST.type_ ty)
          | None -> ""
        in
        let init_str =
          match vdef.IL.vinit with
          | Some e -> spf " = %s" (pretty_exp e)
          | None -> ""
        in
        spf "%s%s%s%s;" ind1 field_name ty_str init_str)
    |> String.concat "\n"
  in
  let methods_str =
    cdef.IL.cmethods
    |> List.map (fun (ent, fdef) ->
        let method_name = pretty_entity_name ent.IL.name in
        pretty_function_definition ~name:method_name ~indent:(indent + 1) fdef)
    |> String.concat "\n\n"
  in
  let fixmes_str =
    if cdef.IL.cfixmes = [] then ""
    else
      "\n" ^ ind1
      ^ spf "// %d fixme fields/params" (List.length cdef.IL.cfixmes)
  in
  let body =
    [ fields_str; methods_str; fixmes_str ]
    |> List.filter (fun s -> s <> "")
    |> String.concat "\n\n"
  in
  let header_indent = if inline then "" else ind in
  spf "%sclass %s%s%s {\n%s\n%s}" header_indent name extends_str implements_str
    body ind

and pretty_definition ?(indent = 0) (ent, def_kind) =
  let name = pretty_entity_name ent.IL.name in
  match def_kind with
  | IL.FuncDef fdef -> pretty_function_definition ~name ~indent fdef
  | IL.ClassDef cdef -> pretty_class_definition ~name ~indent cdef
  | IL.FixmeDef -> indent_str indent ^ spf "// FIXME_DEF: %s" name

(*****************************************************************************)
(* Nodes *)
(*****************************************************************************)

(* Format a line annotation like " @l.42" from a token *)
let pretty_tok_loc tok =
  match Tok.loc_of_tok tok with
  | Ok loc -> spf " @l.%d" loc.Loc.pos.Pos.line
  | Error _ -> ""

(* Extract location annotation from a node *)
let node_loc_annot = function
  | IL.Enter
  | IL.Exit
  | IL.Join ->
      ""
  | IL.TrueNode e
  | IL.FalseNode e ->
      pretty_orig_annot e.IL.eorig
  | IL.NInstr instr -> pretty_orig_annot instr.IL.iorig
  | IL.NCond (tok, _)
  | IL.NGoto (tok, _)
  | IL.NReturn (tok, _)
  | IL.NThrow (tok, _) ->
      pretty_tok_loc tok
  | IL.NMatch name -> pretty_tok_loc (snd name.IL.ident)
  | IL.NCase (name, _) -> pretty_tok_loc (snd name.IL.ident)
  | IL.NNestedDef ent -> (
      match ent.IL.name with
      | IL.EN name -> pretty_tok_loc (snd name.IL.ident)
      | IL.FixmeEntity _ -> "")
  | IL.NOther _ -> ""
  | IL.NTodo _ -> ""

let pretty_node_kind = function
  | IL.Enter -> "Enter"
  | IL.Exit -> "Exit"
  | IL.TrueNode e -> spf "TrueNode(%s)" (pretty_exp e)
  | IL.FalseNode e -> spf "FalseNode(%s)" (pretty_exp e)
  | IL.Join -> "Join"
  | IL.NInstr instr -> pretty_instr_kind instr.IL.i
  | IL.NCond (_, e) -> spf "Cond(%s)" (pretty_exp e)
  | IL.NGoto (_, (lbl, _)) -> spf "Goto(%s)" (fst lbl)
  | IL.NReturn (_, e) -> spf "Return(%s)" (pretty_exp e)
  | IL.NThrow (_, e) -> spf "Throw(%s)" (pretty_exp e)
  | IL.NMatch name -> spf "Match(%s)" (fst name.IL.ident)
  | IL.NCase (name, pattern) ->
      let pat_str =
        match pattern with
        | IL.PatLiteral lit -> G.show_literal lit
        | IL.PatWildcard -> "_"
        | IL.PatVariable n -> fst n.IL.ident
        | IL.PatConstructor (n, args) ->
            let args_str =
              args |> List.map (fun a -> fst a.IL.ident) |> String.concat ", "
            in
            spf "%s(%s)" (fst n.IL.ident) args_str
      in
      spf "Case(%s, %s)" (fst name.IL.ident) pat_str
  | IL.NNestedDef ent -> spf "<def> %s { ... }" (pretty_entity_name ent.IL.name)
  | IL.NOther other -> (
      match other with
      | IL.DefStmt (ent, _) -> spf "// def %s" (pretty_entity_name ent.IL.name)
      | IL.DirectiveStmt _ -> "// <directive>"
      | IL.Noop msg -> spf "// noop: %s" msg)
  | IL.NTodo stmt -> spf "TODO(%s)" (pretty_stmt stmt)

let pretty_node n =
  let kind_str = pretty_node_kind n.IL.n in
  let loc_str = node_loc_annot n.IL.n in
  let at_exit_str = if n.IL.at_exit then " @exit" else "" in
  kind_str ^ at_exit_str ^ loc_str

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

let pretty_program prog = pretty_stmts ~indent:0 prog

(*****************************************************************************)
(* Exposed API *)
(*****************************************************************************)

let name = pretty_name
let lval = pretty_lval
let exp = pretty_exp
let instr = pretty_instr
let stmt = pretty_stmt
let stmts = pretty_stmts
let node = pretty_node
let function_definition = pretty_function_definition
let class_definition = pretty_class_definition
let definition = pretty_definition
let program = pretty_program
