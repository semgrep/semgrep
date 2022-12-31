(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Ast_c
open Ast_cil
module A = Ast_c
module A2 = Ast_cpp
module D = Datalog_code
module G = Graph_code
module E = Entity_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Generating dataflow-related datalog facts for C.
 *
 * See pfff/mini/datalog_minic.ml for more comments, history, and notes.
 * Lots of code in this file is copy pasted from datalog_minic.ml
 * (but now actually improved, e.g. with the notion of lvalue/rvalue).
 *
 * todo:
 *  - could also add the AST of macros in the environment to
 *    expand sometimes
 *  - less: could split in different files, ast_cil.ml, ast_cil_build.ml,
 *    datalog_c.ml, datalog_c_hooks.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type fact = Datalog_code.fact

type env = {
  scope : string; (* qualifier, usually the current function *)
  c_file_readable : Common.filename;
  long_format : bool;
  globals : Graph_code.t; (* we may also want the AST of macros *)
  (* Because of the trick we use in graph_code_c for e.g. renaming
   * static functions to avoid name collisions,
   * you need to use this function each time you think
   * a name refers to a global entity.
   *)
  globals_renames : Ast_c.name -> Ast_c.name;
  (* Have option type because of macro parameters ... could have a TAny also.
   * Need a ref because instrs_of_expr will add new local variables.
   *)
  locals : (string * type_ option) list ref;
  (* the output *)
  facts : fact list ref;
}

(* less: type format = Classic | Bddbddb | BddbddbLong ? *)

let long_format = ref true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let debug any =
  let s = Ast_c.show_any any in
  (*
  let ii = Lib_parsing_c.ii_of_any any in
  pr2 (spf "PB: %s" (Parse_info.string_of_info (List.hd ii)));
*)
  pr2 s

(* location are unique normally so nice to generate unique readable names *)
let loc_of env tok =
  let line = Parse_info.line_of_info tok in
  let col = Parse_info.col_of_info tok in
  if env.long_format then spf "%d:%d" line col
    (* lua datalog does not handle ':' *)
  else spf "line_%d_col_%d" line col

let str_of t = Parse_info.str_of_info t

(* for int* x[10][10] we want to generate a list of Alloc and
 * we want to create fresh array each time
 * (we could use gensym for that though).
 * TODO: good enough with the way types are represented? Need
 * Ast_c.Pointer of tok * type_ ?
 *)
let tok_of_type _t = raise Todo
(*
  List.hd (Lib_parsing_c.ii_of_any (A.Type t))
*)

let tokwrap_of_expr _e = raise Todo
(*
  (), List.hd (Lib_parsing_c.ii_of_any (A.Expr e))
*)

let var_of_instr instr =
  match instr with
  | Assign (v, _)
  | AssignAddress (v, _)
  | AssignLvalue (_, v) ->
      v

exception NotSimpleExpr

let string_of_op _str = "_op_todo"

let is_local env s =
  Common.find_opt (fun (x, _) -> x =$= s) !(env.locals) <> None

let unbracket (_, x, _) = x

(*****************************************************************************)
(* Normalize *)
(*****************************************************************************)

(* The goal is to transform constructs from Ast_c to simpler constructs.
 * Here are the simplifications done:
 * - linearization of expression
 * - sugar removal for postfix/infix increments
 * - ???
 *)

(* could also use gensym() *)
let counter = ref 0

(* note that we still use var_of_name to generate the extra scope info
 * so no need to add it there
 *)
let fresh_var env (_, tok) =
  incr counter;
  let s = spf "_v_%d" (* env.scope? no! *) !counter in
  (* todo type! *)
  env.locals := (s, None) :: !(env.locals);
  (s, tok)

(* Need env.globals to unsugar certain calls like error(xxx) when error
 * is actually a global it's really a  ( *error)(xxx).
 * Need also to know the current set of locals, so need both local
 * and global analysis results.
 *)
let instrs_of_expr env e =
  let instrs = ref [] in

  (* let _new_locals = ref [] with their types? ... *)
  let rec instr_of_expr e =
    match e with
    | A.Int _
    | A.Float _
    | A.String _
    | A.Char _
    | A.Bool _
    | A.Null _
    | A.ConcatString _
    | A.Id _
    | A.Unary (_, (A2.DeRef, _))
    | A.Call _
    | A.ArrayAccess _
    | A.RecordPtAccess _
    | A.Defined _
    | A.Binary _
    | A.Unary (_, ((A2.UnPlus | A2.UnMinus | A2.Tilde | A2.Not), _))
    | A.SizeOf _
    | A.GccConstructor _
    | A.Ellipses _
    | A.DeepEllipsis _
    | A.TypedMetavar _ ->
        (* less: we could generate a special fresh_var that datalog would not
         * have to track? hmm but maybe the var is actually used
         * by code using var_of_instr().
         *)
        Assign (fresh_var env (tokwrap_of_expr e), rvalue_of_simple_expr e)
    (* todo: actually an alloc is hidden there! *)
    | A.Assign (op, e1, A.ArrayInit xs) ->
        let ys =
          xs |> unbracket
          |> List.map (fun (idxopt, value) ->
                 (* less? recompute e1 each time? should store in intermediate val? *)
                 let tok =
                   match op with
                   | A2.SimpleAssign tok -> tok
                   | A2.OpAssign (_, tok) -> tok
                 in
                 let access =
                   match idxopt with
                   | Some e -> A.ArrayAccess (e1, PI.fake_bracket tok e)
                   | None ->
                       A.ArrayAccess
                         (e1, PI.fake_bracket tok (A.Int (Some 0, tok)))
                 in
                 A.Assign (op, access, value))
        in
        let seq = Common2.foldl1 (fun e rest -> Sequence (e, rest)) ys in
        instr_of_expr seq
    (* todo: actually an alloc is hidden there! *)
    | A.Assign (op, e1, A.RecordInit xs) ->
        let ys =
          xs |> unbracket
          |> List.map (fun (name, value) ->
                 let tok =
                   match op with
                   | A2.SimpleAssign tok -> tok
                   | A2.OpAssign (_, tok) -> tok
                 in
                 (* less? recompute e1 each time? should store in intermediate val? *)
                 let access =
                   A.RecordPtAccess
                     ( A.Unary (e1, (A2.GetRef, tok)),
                       Parse_info.fake_info tok "->",
                       name )
                 in
                 A.Assign (op, access, value))
        in
        let seq = Common2.foldl1 (fun e rest -> Sequence (e, rest)) ys in
        instr_of_expr seq
    (* ok, an actual instr! For our analysis we don't care about op (we are
     * not even control flow sensitive anyway)
     *)
    | A.Assign (_op, e1, e2) -> (
        let lv = lvalue_of_expr e1 in

        match (lv, e2) with
        | Id v, A.Unary (e, (A2.GetRef, _)) -> (
            match lvalue_of_expr e with
            (* less: what &( *x ) means? *)
            | DeRef _ ->
                debug (A.Expr e);
                raise Impossible
            | lv -> AssignAddress (v, lv))
        | _ -> (
            match lv with
            | Id name ->
                Assign
                  ( name,
                    try rvalue_of_simple_expr e2 with
                    | NotSimpleExpr -> Lv (Id (var_of_expr e2)) )
            | lv -> AssignLvalue (lv, var_of_expr e2)))
    | A.Unary (e, (A2.GetRef, tok)) -> (
        let v = fresh_var env ((), tok) in
        let lv = lvalue_of_expr e in
        match lv with
        | DeRef _ ->
            debug (A.Expr e);
            raise Impossible
        | lv -> AssignAddress (v, lv))
    | A.Unary (_, (A2.GetRefLabel, _)) ->
        (* ast_c_build should forbid that gccext *)
        debug (A.Expr e);
        raise Impossible
    | A.Sequence (e1, e2) ->
        let i1 = instr_of_expr e1 in
        Common.push i1 instrs;
        instr_of_expr e2
    | A.Cast (_tTODO, e) -> instr_of_expr e
    (* for pointer analysis we don't care to respect the exact semantic, we
     * are not even control flow sensitive
     *)
    | A.Postfix (e, _op)
    | A.Infix (e, _op) ->
        instr_of_expr e
    (* Could try to expand to a '_builtin_cond(e1, e2, e3)' but
     * what would be the type of this function? bool -> T -> T -> T ...
     * need polymorphic type. So for now just expand to
     * 'v1 = e1; v2 = e2; v2 = e3;'
     *)
    | A.CondExpr (e1, e2, e3) ->
        let i1 = instr_of_expr e1 in
        Common.push i1 instrs;
        let tokwrap = tokwrap_of_expr e2 in
        let v = fresh_var env tokwrap in
        let tok = snd tokwrap in
        let i2 =
          instr_of_expr (A.Assign (Ast_cpp.SimpleAssign tok, A.Id v, e2))
        in
        Common.push i2 instrs;
        instr_of_expr (A.Assign (Ast_cpp.SimpleAssign tok, A.Id v, e3))
        (* like GccConstructor can be outside Assign context when in macro *)
        (* todo: an alloc is hidden here?? *)
    | A.ArrayInit _
    | A.RecordInit _ ->
        debug (A.Expr e);
        let tokwrap = tokwrap_of_expr e in
        let v = fresh_var env tokwrap in
        let tok = snd tokwrap in
        instr_of_expr (A.Assign (Ast_cpp.SimpleAssign tok, A.Id v, e))
  and rvalue_of_simple_expr e =
    match e with
    | A.Int x -> Int x
    | A.Float x -> Float x
    | A.String x -> String x
    | A.Char x -> String x
    (* could be lots of things, global, local, param, constant, function! *)
    | A.Id name -> Lv (Id name)
    | A.Unary (e, (A2.DeRef, _)) -> Lv (DeRef (var_of_expr e))
    (* todo: xalloc, smalloc, and other wrappers? *)
    | A.Call (A.Id ("malloc", tok), (_, es, _)) -> (
        match es with
        | [ Arg (SizeOf (_, Right t)) ] -> Alloc t
        | [
         Arg (Binary (e, (Ast_cpp.Arith Ast_cpp.Mul, _), SizeOf (_, Right t)));
        ] ->
            let v = var_of_expr e in
            AllocArray (v, t)
        | [ Arg (SizeOf (_, Left _e)) ] ->
            (* todo: need potentially to resolve the type of e *)
            (* debug (Expr e); *)
            Alloc (A.TBase ("_unknown_", tok))
        | _ ->
            debug (Expr e);
            Alloc (A.TBase ("_unknown_", tok)))
    | A.Call (e, (_, es, _)) -> (
        let vs = List.map var_of_arg es in
        match e with
        | A.Id name ->
            if
              is_local env (fst name)
              (* fn(...) when fn is a local is really a  ( *fn)(...) *)
            then DynamicCall (var_of_expr e, vs)
            else
              (* fn(...) is actually sugar when fn is a global *)
              let name = env.globals_renames name in
              let str = fst name in
              if
                (not (G.has_node (str, E.Function) env.globals))
                && (not (G.has_node (str, E.Macro) env.globals))
                && G.has_node (str, E.Global) env.globals
              then DynamicCall (var_of_expr e, vs)
                (* could assert there is E.Function or E.Macro or E.Prototype *)
              else StaticCall (name, vs)
        (* ( *f)(...) *)
        | A.Unary (e, (A2.DeRef, _)) -> DynamicCall (var_of_expr e, vs)
        (* x->f(...) is actually sugar for ( *  x->f)(...) *)
        | A.RecordPtAccess (_, _, _)
        (* x[y](...) is also sugar for ( * x[y](...) *)
        | A.ArrayAccess (_, _) ->
            DynamicCall (var_of_expr e, vs)
        | _ ->
            debug (Expr e);
            raise Todo)
    | A.Binary (e1, (_op, tok), e2) ->
        let vs = List.map var_of_expr [ e1; e2 ] in
        BuiltinCall (("_builtin_" ^ string_of_op tok, tok), vs)
    | A.Unary (e, ((A2.UnPlus | A2.UnMinus | A2.Tilde | A2.Not), tok)) ->
        let vs = [ var_of_expr e ] in
        BuiltinCall (("_builtin_" ^ string_of_op tok, tok), vs)
    | A.ArrayAccess (e1, (_, e2, _)) ->
        let v1 = var_of_expr e1 in
        let v2 = var_of_expr e2 in
        Lv (ArrayAccess (v1, v2))
    | A.RecordPtAccess (e, _, name) ->
        let v = var_of_expr e in
        Lv (ObjField (v, name))
    | A.SizeOf (_t, Left e) ->
        let instr = instr_of_expr e in
        Common.push instr instrs;
        Int ((*"0_sizeof"*) None, tokwrap_of_expr e |> snd)
    | A.SizeOf (_t, Right t) -> Int ((*"0_sizeof"*) None, tok_of_type t)
    (* can be in macro context, e.g. #define SEG (struct x) { ... } *)
    | A.GccConstructor (t, _eTODO) -> Alloc t
    | _ ->
        (* hmmm maybe better to have this function return a rvalue option *)
        raise NotSimpleExpr
  and var_of_arg x =
    match x with
    | Arg e -> var_of_expr e
  and var_of_expr e =
    match e with
    | A.Id name -> name
    | _ ->
        let instr = instr_of_expr e in
        Common.push instr instrs;
        var_of_instr instr
  and lvalue_of_expr e =
    try
      match rvalue_of_simple_expr e with
      | Lv x -> x
      | _ -> Id (var_of_expr e)
    with
    | NotSimpleExpr -> Id (var_of_expr e)
  in

  let i = instr_of_expr e in
  List.rev (i :: !instrs)

(*****************************************************************************)
(* Abstract memory locations *)
(*****************************************************************************)

let var_of_global env name =
  let name = env.globals_renames name in
  let s = fst name in
  (*
  if Common.find_opt (fun (x,_) -> x =$= s) env.globals = None
  then error (spf "unknown global: %s" s) name;
*)
  if env.long_format then (
    (* bug: no!! spf "%s#%s" env.c_file_readable
     * we must actually get the file at definition time, not use time
     *)
    let candidates =
      [ E.Macro; E.Constant; E.Function; E.Constructor; E.Global ]
    in
    let res =
      candidates
      |> Common.map_filter (fun kind ->
             if G.has_node (s, kind) env.globals then Some (s, kind) else None)
    in
    match res with
    | [ node ] ->
        let file = G.file_of_node node env.globals in
        spf "%s#%s" file s
    | x :: y :: xs ->
        pr2
          (spf "Conflicting entities for %s [%s]" s
             (x :: y :: xs |> List.map G.string_of_node |> Common.join ","));
        let file = G.file_of_node x env.globals in
        spf "%s#%s" file s
    (* maybe a prototype or extern *)
    | [] ->
        (match () with
        | _ when s =~ "_builtin_.*" -> ()
        | _ ->
            if
              G.has_node (s, E.Prototype) env.globals
              || G.has_node (s, E.GlobalExtern) env.globals
              (* todo: could print a warning to force people to give
               * a "model" for the external or asm function
               *)
            then ()
            else
              pr2_once
                (spf "Could not find any definition nor prototype for %s" s));
        s)
  else s

let var_of_local env name =
  if env.long_format then
    spf "%s#%s:%s" env.c_file_readable env.scope (fst name)
  else spf "%s__%s" env.scope (fst name)

let var_of_name env var_or_name =
  let s = fst var_or_name in
  match Common.find_opt (fun (x, _) -> x =$= s) !(env.locals) with
  | None -> var_of_global env var_or_name
  | Some _t -> var_of_local env var_or_name

(* the variable name is also its heap abstract location as in C
 * you can get the address of any local variables.
 *)
let heap_of_name env var_or_name = var_of_name env var_or_name

(* heap location, abstract memory location, heap abstraction, etc *)
let heap_of_cst env name =
  spf "_val_of_%s_%s" (fst name) (loc_of env (snd name))

let heap_of_malloc env t =
  let tok = tok_of_type t in
  spf "_malloc_in_%s_%s" env.scope (loc_of env tok)

let heap_of_malloc_array env t =
  let tok = tok_of_type t in
  (* old: used to have
   * let pt =  spf "_array_in_%s_%s" env.scope (loc_of env tok) in
   * let pt2 = spf "_array_elt_in_%s_%s" env.scope (loc_of env tok) in
   * and an array_point_to/2 but it does not work
   *)
  spf "_array_elt_in_%s_%s" env.scope (loc_of env tok)

let invoke_loc_of_name env name =
  if env.long_format then
    spf "%s#%s" env.c_file_readable (loc_of env (snd name))
  else spf "_in_%s_%s" env.scope (loc_of env (snd name))

(* TODO: need to look for type of v in env to actually qualify ... *)
let fully_qualified_field _env _v fldname =
  let fld = fst fldname in
  spf "_fld__%s" fld

(* TODO: need to use _struct at some point *)
let fully_qualified_field_of_struct _struc fld = spf "_fld__%s" fld

(*****************************************************************************)
(* Fact generation *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Instr *)
(* ------------------------------------------------------------------------- *)

let facts_of_instr env = function
  | Assign (var, e) -> (
      let dest = var_of_name env var in
      match e with
      | Int x -> [ D.PointTo (dest, spf "_int__%s" (str_of (snd x))) ]
      | Float x -> [ D.PointTo (dest, spf "_float__%s" (loc_of env (snd x))) ]
      | String x -> [ D.PointTo (dest, spf "_str__%s" (loc_of env (snd x))) ]
      (* like in miniC *)
      | StaticCall (("printf", _), _args) -> []
      | StaticCall (name, args)
      | DynamicCall (name, args)
      | BuiltinCall (name, args) -> (
          let invoke = invoke_loc_of_name env name in
          (args |> Common.index_list_1
          |> List.map (fun (v, i) -> D.Argument (invoke, i, var_of_name env v))
          )
          @ [ D.ReturnValue (invoke, dest) ]
          @
          match e with
          | StaticCall _
          | BuiltinCall _ ->
              [ D.CallDirect (invoke, var_of_global env name) ]
          | DynamicCall _ -> [ D.CallIndirect (invoke, var_of_name env name) ]
          | _ -> raise Impossible)
      (* TODO: could be enum or constant! lookup g *)
      | Lv (Id name) -> [ D.Assign (dest, var_of_name env name) ]
      | Lv (DeRef var2) -> [ D.AssignContent (dest, var_of_name env var2) ]
      | Lv (ObjField (var2, fld)) ->
          [
            D.AssignLoadField
              (dest, var_of_name env var2, fully_qualified_field env var2 fld);
          ]
      | Lv (ArrayAccess (var2, _vidx)) ->
          (* less: could also add info that vidx must be an int *)
          [ D.AssignArrayElt (dest, var_of_name env var2) ]
      | Alloc t ->
          let pt = heap_of_malloc env t in
          [ D.PointTo (dest, pt) ]
      | AllocArray (_v, t) ->
          let pt = heap_of_malloc_array env t in
          [ D.PointTo (dest, pt) ])
  | AssignLvalue (ArrayAccess (varr, _vidx), vval) ->
      (* less: could also add info that vidx must be an int *)
      [ D.AssignArrayDeref (var_of_name env varr, var_of_name env vval) ]
  | AssignLvalue (ObjField (var, fld), var2) ->
      [
        D.AssignStoreField
          ( var_of_name env var,
            fully_qualified_field env var2 fld,
            var_of_name env var2 );
      ]
  | AssignLvalue (DeRef var, var2) ->
      [ D.AssignDeref (var_of_name env var, var_of_name env var2) ]
  | AssignAddress (var, Id name) ->
      [ D.AssignAddress (var_of_name env var, heap_of_name env name) ]
  | AssignAddress (var, ArrayAccess (varray, _vidx)) ->
      (* less: could also add info that vidx must be an int *)
      [
        D.AssignArrayElementAddress (var_of_name env var, var_of_name env varray);
      ]
  | AssignAddress (v, ObjField (vobj, fld)) ->
      [
        D.AssignFieldAddress
          ( var_of_name env v,
            var_of_name env vobj,
            fully_qualified_field env vobj fld );
      ]
  | AssignAddress (_var, DeRef _) -> raise Impossible
  | AssignLvalue (Id _name, _var) -> raise Impossible

(* ------------------------------------------------------------------------- *)
(* Return *)
(* ------------------------------------------------------------------------- *)

let return_fact env instr =
  let var = var_of_instr instr in
  D.Assign (spf "ret_%s" env.scope, var_of_name env var)

(* ------------------------------------------------------------------------- *)
(* Defs *)
(* ------------------------------------------------------------------------- *)
let rec facts_of_def env x =
  match x with
  | DefStmt x -> facts_of_definition env x
  | DirStmt x -> facts_of_directive env x
  | _ -> raise Impossible

and facts_of_directive env def =
  match def with
  | Define (_, name, _body) ->
      [ D.PointTo (var_of_global env name, heap_of_cst env name) ]
  | Macro _ ->
      (* todo? *)
      []
  | Include _
  | OtherDirective _ ->
      raise Impossible

and facts_of_definition env def =
  match def with
  | StructDef def ->
      def.s_flds |> PI.unbracket
      |> Common.map_filter (fun fld ->
             match fld.fld_name with
             (* todo: kencc ext field! *)
             | None -> None
             | Some name -> (
                 match fld.fld_type with
                 | TBase _ ->
                     Some
                       (D.PointTo
                          ( fully_qualified_field_of_struct (fst def.s_name)
                              (fst name),
                            heap_of_cst env name ))
                 (* TODO: like for Global, if fields is an array, we should
                  * do an hidden alloc! see mini/struct_array.c
                  *)
                 | _ -> None))
  | EnumDef def ->
      let { e_name = _name; e_consts = xs } = def in
      xs
      |> List.map (fun (name, _eopt) ->
             D.PointTo (var_of_global env name, heap_of_cst env name))
  | FuncDef def ->
      let _ret, params = def.f_type in
      (params |> Common.index_list_1
      |> Common.map_filter (function
           | ParamDots _, _ -> None
           | ParamClassic p, i -> (
               match p.p_name with
               | None -> None
               | Some name ->
                   Some
                     (D.Parameter
                        (var_of_global env def.f_name, i, var_of_local env name))
               )))
      @
      (* less: could skip when return void *)
      let name = env.globals_renames def.f_name in
      [
        D.Return (var_of_global env def.f_name, spf "ret_%s" (fst name));
        (* This is because in C there is some sugar on assign on functions.
         * One can do trapenable(myfunc) instead of trapenable(&myfunc),
         * so in such case it's simpler to consider every funcname as
         * a pointer to itself.
         *)
        D.PointTo (var_of_global env def.f_name, var_of_global env def.f_name);
      ]
  | VarDef var ->
      let name = var.v_name in

      let rec aux current_v current_type =
        match current_type with
        (* int* x[...]; <=> x = malloc(n*sizeof(int* ) *)
        | TArray (_eopt, t) -> (
            let vsize_dontcare = fresh_var env ((), tok_of_type t) in
            let instr = Assign (current_v, AllocArray (vsize_dontcare, t)) in
            facts_of_instr env instr
            @
            (* hmm got int* x[...][...] need to recurse *)
            match t with
            | TArray _ ->
                (* new_v = x[0]; new_v = malloc(n*sizeof(int * )) *)
                let new_v = fresh_var env ((), tok_of_type t) in
                let vidx_dontcare = fresh_var env ((), tok_of_type t) in
                let instr_index =
                  Assign (new_v, Lv (ArrayAccess (current_v, vidx_dontcare)))
                in
                facts_of_instr env instr_index @ aux new_v t
            | _ -> [])
        (*    | TBase _ -> [D.PointTo (var_of_global env name, heap_of_cst env name)]*)
        | _ -> []
      in
      aux name var.v_type
  | TypeDef _
  | Prototype _ ->
      raise Impossible
