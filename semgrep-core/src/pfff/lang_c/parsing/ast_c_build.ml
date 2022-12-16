(* Yoann Padioleau
 *
 * Copyright (C) 2012, 2014 Facebook
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

open Ast_cpp
module A = Ast_c
module PI = Parse_info

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Ast_cpp to Ast_c_simple.
 *
 * We skip the then part of ifdefs.
 *
 * todo:
 *  - lift up local union and struct defined in functions?
 *    (hmm but better to rewrite the code I think)
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(* for anon struct, which is dangerous! because the main function
 * will return different results given the same input when called
 * two times in a row
*)
let cnt = ref 0

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

exception ObsoleteConstruct of string * Parse_info.t
exception CplusplusConstruct
exception TodoConstruct of string * Parse_info.t
exception CaseOutsideSwitch
exception MacroInCase

type env = {
  mutable struct_defs_toadd: A.struct_def list;
  mutable enum_defs_toadd: A.enum_def list;
  mutable typedefs_toadd: A.type_def list;
}

let empty_env () = {
  struct_defs_toadd = [];
  enum_defs_toadd = [];
  typedefs_toadd = [];
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let gensym_struct cnt =
  spf "__anon_struct_%d" cnt

let gensym_enum cnt =
  spf "__anon_enum_%d" cnt

let debug any =
  logger#debug "%s" (Ast_cpp.show_any any)

let rec ifdef_skipper xs f =

  match xs with
  | [] -> []
  | x::xs ->
      (match f x with
       | None -> x::ifdef_skipper xs f
       | Some ifdef ->
           (match ifdef with
            | Ifdef tok ->
                logger#info "skipping: %s" (Parse_info.str_of_info tok);
                (try
                   let (_, x, rest) =
                     xs |> Common2.split_when (fun x ->
                       match f x with
                       | Some (IfdefElse _) -> true
                       | Some (IfdefEndif _) -> true
                       | _ -> false
                     )
                   in
                   (match f x with
                    | Some (IfdefEndif _) ->
                        ifdef_skipper rest f
                    | Some (IfdefElse _) ->
                        let (before, _x, rest) =
                          rest |> Common2.split_when (fun x ->
                            match f x with
                            | Some (IfdefEndif _) -> true
                            | _ -> false
                          )
                        in
                        ifdef_skipper before f @ ifdef_skipper rest f
                    | _ -> raise Impossible
                   )
                 with Not_found ->
                   failwith (spf "%s: unclosed ifdef" (Parse_info.string_of_info tok))
                )
            | (IfdefElse tok | IfdefElseif (tok) | IfdefEndif tok) ->
                failwith (spf "%s: no ifdef" (Parse_info.string_of_info tok))
           )
      )

let bracket_keep of_a (t1, x, t2) = (t1, of_a x, t2)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program xs =
  let env = empty_env () in
  toplevels env xs |> List.flatten

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevels env xs =
  ifdef_skipper xs (function CppIfdef x -> Some x | _ -> None)
  |> List.map (toplevel env)

and toplevel env x =
  match x with
  | X (D decl) ->
      declaration env decl |> List.map (fun x -> A.DefStmt x)
  | X (S st) ->
      [stmt env st]
  | CppDirective x ->
      [A.DirStmt (cpp_directive env x)]

  | (MacroVar (_, _)|MacroDecl (_, _, _, _)) -> raise Todo
  | CppIfdef _ -> raise Impossible (* see ifdef_skipper *)


and declaration env x =
  match x with
  | Func def -> [A.FuncDef (func_def env def)]

  (* was in block_declaration before *)
  | DeclList (xs, _) ->
      let xs = Common.map_filter (onedecl env) xs in

      let structs = env.struct_defs_toadd in
      let enums = env.enum_defs_toadd in
      let typedefs = env.typedefs_toadd in
      env.struct_defs_toadd <- [];
      env.enum_defs_toadd <- [];
      env.typedefs_toadd <- [];
      (structs |> List.map (fun x -> A.StructDef x)) @
      (enums |> List.map (fun x -> A.EnumDef x)) @
      (typedefs |> List.map (fun x -> A.TypeDef x)) @
      (xs |> List.map (fun x ->
         (* could skip extern declaration? *)
         match x with
         | { A.v_type = A.TFunction ft; v_storage = storage; _ } ->
             A.Prototype { A.
                           f_name = x.A.v_name;
                           f_type = ft;
                           f_static = (storage =*= A.Static);
                           f_body = PI.fake_bracket (snd x.A.v_name) [];
                         }
         | _ -> A.VarDef x
       ))


  (* todo *)
  | Asm (_tok1, _volatile_opt, _asmbody, _tok2) ->
      raise Todo
  | EmptyDef _ -> []

  | UsingDecl _ | NamespaceAlias _ | StaticAssert _
  | Namespace (_, _, _)
  | ExternList (_, _, _)|ExternDecl (_, _, _)
  | TemplateDecl _ | TemplateInstanciation _ ->
      debug (Toplevel (X (D x))); raise CplusplusConstruct
  | DeclTodo _ ->
      debug (Toplevel (X (D x))); raise Todo
  (* not much we can do here, at least the parsing statistics should warn the
   * user that some code was not processed
  *)
  | NotParsedCorrectly _ -> []


(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)
and func_def env ({name = f_name; specs = _specsTODO}, def) =
  { A.
    f_name = name env f_name;
    f_type = function_type env def.f_type;
    f_static =
      (match def.f_specs with
       | [ST (Static, _)] -> true
       | _ -> false
      );
    f_body = function_body env def.f_body;
  }

and function_body env x =
  match x with
  | FBDef x -> compound env x
  | FBDecl sc -> sc, [], sc
  | FBDelete _ | FBDefault _ | FBZero _ -> raise CplusplusConstruct

and function_type env x =
  match x with
    { ft_ret = ret;
      ft_params = params;
      ft_specs = _TODO;
      ft_const = const;
      ft_throw = throw;
    } ->
      (match const, throw with
       | None, [] -> ()
       | _ -> raise CplusplusConstruct
      );

      (full_type env ret,
       List.map (parameter env) (params |> unparen)
      )

and parameter env x =
  match x with
  | P { p_name = n;
        p_type = t;
        p_val = v;
        p_specs = _TODO;
      } ->
      (match v with
       | None -> ()
       | Some _ -> debug (Parameter x); raise CplusplusConstruct
      );
      A.ParamClassic { A.
                       p_name =
                         (match n with
                          (* probably a prototype where didn't specify the name *)
                          | None -> None
                          | Some name -> Some name
                         );
                       p_type = full_type env t;
                     }
  | ParamEllipsis t -> A.ParamDots t
  | ParamVariadic _ | ParamTodo _ ->
      debug (Parameter x); raise CplusplusConstruct


(* ---------------------------------------------------------------------- *)
(* Variables *)
(* ---------------------------------------------------------------------- *)
and onedecl env d =
  match d with
  | EmptyDecl ft ->
      (match Ast_cpp.unwrap_typeC ft with
       (* it's ok to not have any var decl as long as a type
        * was defined. struct_defs_toadd should not be empty then.
       *)
       | ClassDef _ | EnumDef _ ->
           let _ = full_type env ft in
           None
       (* forward declaration *)
       | ClassName _ ->
           None

       | _ -> debug (OneDecl d); raise Todo
      )
  | TypedefDecl (_t, ft, id) ->
      let def = { A.t_name = id; t_type = full_type env ft } in
      env.typedefs_toadd <- def :: env.typedefs_toadd;
      None

  | V ({ name = n; specs = specs_and_sto}, { v_init = iopt; v_type = ft}) ->
      let init_opt =
        match iopt with
        | None -> None
        | Some (EqInit (_, ini)) -> Some (initialiser env ini)
        | Some (Bitfield _) -> raise Todo
        | Some (ObjInit _) ->
            debug (OneDecl d);
            raise CplusplusConstruct
      in
      Some { A.
             v_name = name env n;
             v_type = full_type env ft;
             v_storage = storage_in_specs env specs_and_sto;
             v_init = init_opt;
           }
  | StructuredBinding _ -> raise CplusplusConstruct
  (* should happen only inside fields, and should be covered in fieldkind *)
  | BitField _ -> raise Impossible

and initialiser env x =
  match x with
  | InitExpr e -> expr env e
  | InitList xs ->
      (match xs |> unparen with
       | [] -> debug (Init x); raise Impossible
       | (InitDesignators ([DesignatorField (_, _)], _, _init))::_ ->
           A.RecordInit (bracket_keep (fun xs ->
             xs |> List.map (function
               | InitDesignators ([DesignatorField (_, ident)], _, init) ->
                   ident, initialiser env init
               | _ -> debug (Init x); raise Todo
             )) xs)
       | _ ->
           A.ArrayInit (bracket_keep (fun xs ->
             xs |> Common.map (function
               (* less: todo? *)
               | InitIndexOld ((_, idx, _), ini) ->
                   Some (expr env idx), initialiser env ini
               | InitDesignators([DesignatorIndex(_, idx, _)], _, ini) ->
                   Some (expr env idx), initialiser env ini
               | x -> None, initialiser env x
             )) xs)
      )
  (* should be covered by caller *)
  | InitDesignators _ -> debug (Init x); raise Todo
  | InitIndexOld _ | InitFieldOld _ -> debug (Init x); raise Todo

(* TODO: filter and focus on ST *)
and storage_in_specs _env xs =
  match xs with
  | [ST (y, _)] ->
      (match y with
       | Static -> A.Static
       | Extern -> A.Extern
       | Auto | Register -> A.DefaultStorage
       | StoInline -> raise CplusplusConstruct
      )
  | [] | _ -> A.DefaultStorage

(* ---------------------------------------------------------------------- *)
(* Cpp *)
(* ---------------------------------------------------------------------- *)

and cpp_directive env x =
  match x with
  | Define (tok, name, def_kind, def_val) ->
      let v = cpp_def_val x env def_val in
      (match def_kind with
       | DefineVar ->
           A.Define (tok, name, v)
       | DefineMacro(args) ->
           A.Macro(tok, name,
                   args |> unparen |> List.map (fun (s, ii) ->
                     (s, ii)
                   ),
                   v)
      )
  | Include (tok, inc_kind) ->
      let s =
        match inc_kind with
        | IncLocal (path, _)  -> "\"" ^ path ^ "\""
        | IncSystem (path, _) -> "<" ^ path ^ ">"
        | IncOther (N ((None, [], IdIdent (x, _t)), _))
          when AST_generic_.is_metavar_name x ->
            x
        | IncOther _ ->
            debug (Cpp x); raise Todo
      in
      A.Include (tok, (s, tok))
  | Undef _ -> debug (Cpp x); raise Todo
  | PragmaAndCo _ -> raise Todo

and cpp_def_val for_debug env x =
  match x with
  | DefineExpr e -> Some (A.CppExpr (expr env e))
  | DefineStmt st -> Some (A.CppStmt (stmt env st))
  | DefineDoWhileZero (_, st, _, _) -> Some (A.CppStmt (stmt env st))
  | DefinePrintWrapper (_, (_, e, _), id) ->
      Some (A.CppExpr (
        A.CondExpr (expr env e,
                    A.Id (name env id),
                    A.Id (name env id))))

  | DefineInit init -> Some (A.CppExpr (initialiser env init))

  | DefineEmpty -> None
  | DefineFunction _
  | DefineType _
  | DefineTodo _
    ->
      debug (Cpp for_debug); raise Todo

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

and expr_or_vars env x =
  match x with
  | Left (Some e, _sc) -> let e = expr env e in Right e
  | _TODO -> Left []

and stmt env st =
  match st with
  | Compound x -> A.Block (compound env x)

  | If (t, _, (_, CondClassic e, _), st1, Some (_, st2)) ->
      A.If (t, expr env e, stmt env st1, Some (stmt env st2))
  | If (t, _, (_, CondClassic e, _), st1, None) ->
      A.If (t, expr env e, stmt env st1, None)
  | Switch (tok, (_, CondClassic e, _), st) ->
      A.Switch (tok, expr env e, cases env st)

  | While (t, (_, CondClassic e, _), st) ->
      A.While (t, expr env e, stmt env st)
  | DoWhile (t, st, _, (_, e, _), _) ->
      A.DoWhile (t, stmt env st, expr env e)
  | For (t, (_, ForClassic (est1, est2, est3), _), st) ->
      A.For (t,
             ForClassic (expr_or_vars env est1,
                         Common2.fmap (expr env) est2,
                         Common2.fmap (expr env) est3),
             stmt env st
            )
  | For (_, (_, ForRange _, _), _) -> raise CplusplusConstruct
  | For (_, (_, ForEllipsis _, _), _) ->
      debug (Stmt st);
      raise Todo

  | MacroIteration _ ->
      debug (Stmt st); raise Todo

  | ExprStmt (eopt, t) ->
      (match eopt with
       | None -> A.Block (PI.fake_bracket t [])
       | Some e -> A.ExprSt (expr env e, t)
      )

  | Label (s, _, st) ->
      A.Label (s, stmt env st)
  | Case _ | CaseRange _ | Default _ ->
      debug (Stmt st); raise CaseOutsideSwitch

  | Jump (j, _) ->
      (match j with
       | Goto (tok, s) -> A.Goto (tok, s)
       | Return (tok, None) -> A.Return (tok, None);
       | Return (tok, Some (Arg e)) -> A.Return (tok, Some (expr env e))
       | Continue tok -> A.Continue tok
       | Break tok -> A.Break tok
       | GotoComputed _ -> debug (Stmt st); raise Todo
       | Return (_tok, Some _) -> raise CplusplusConstruct
      )

  | Try (_, _, _)
  | If (_, _, (_, _, _), _, _)
  | While (_, (_, _, _), _)
  | Switch (_, (_, _, _), _)
    ->
      debug (Stmt st); raise CplusplusConstruct

  | (StmtTodo _ | MacroStmt _ ) ->
      debug (Stmt st); raise Todo

and compound env (t1, xs, t2) =
  t1, (statements_sequencable env xs |> List.flatten), t2

and statements_sequencable env xs =
  ifdef_skipper xs (function CppIfdef x -> Some x | _ -> None)
  |> List.map (statement_sequencable env)


and statement_sequencable env x =
  match x with
  | X (S st) -> [stmt env st]
  | CppDirective x -> debug (Cpp x); raise Todo
  | (MacroVar (_, _)|MacroDecl (_, _, _, _)) -> raise Todo
  | CppIfdef _ -> raise Impossible
  | X (D x) -> [block_declaration env x]

and cases env st =
  match st with
  | Compound (l, xs, r) ->
      let rec aux xs =
        match xs with
        | [] -> []
        | x::xs ->
            (match x with
             (* TODO? what about CaseRange? *)
             | X (S ((Case (_, _, _, sts))))
             | X (S ((Default (_, _, sts))))
               ->
                 let stmts_after_case =
                   sts |> List.map (fun st_or_decl -> X st_or_decl) in
                 (* sts can contain a Case itself, which we want to pack
                  * together *)
                 let xs', rest =
                   (stmts_after_case @ xs) |> Common.span (function
                     | X (S ((Case (_, _, _, _st))))
                     | X (S ((Default (_, _, _st)))) -> false
                     | _ -> true
                   )
                 in
                 let stmts = List.map (function
                   | X (S st) -> stmt env st
                   | x ->
                       debug (Stmt (Compound (l, [x], r)));
                       raise MacroInCase
                 ) xs' in
                 (match x with
                  | X (S ((Case (tok, e, _, _)))) ->
                      A.Case (tok, expr env e, stmts)
                  | X (S ((Default (tok, _, _st)))) ->
                      A.Default (tok, stmts)
                  | _ -> raise Impossible
                 )::aux rest
             | x -> debug (Body (l, [x], r)); raise Todo
            )
      in
      aux xs
  | _ ->
      debug (Stmt st); raise Todo

and block_declaration env block_decl =
  let xs = declaration env block_decl in
  let ys = xs |> Common.map_filter (function
    | A.VarDef x -> Some x
    | _ -> None
  ) in
  Vars ys

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

and expr env e =
  match e with
  | C cst -> constant env cst

  | N (n, _) -> A.Id (name env n)
  | Ellipsis tok -> A.Ellipses tok
  | DeepEllipsis v1 -> let v1 = bracket_keep (expr env) v1 in A.DeepEllipsis v1

  | DotAccess (e, (Dot, t), n) ->
      A.RecordPtAccess (A.Unary (expr env e, (GetRef,t)),
                        t, name env n)
  | DotAccess (e, (Arrow, t), n) ->
      A.RecordPtAccess (expr env e, t, name env n)

  | Cast ((_, ft, _), e) ->
      A.Cast (full_type env ft, expr env e)

  | ArrayAccess (e1, e2) ->
      A.ArrayAccess (expr env e1, bracket_keep (expr env) e2)
  | Binary (e1, op, e2) -> A.Binary (expr env e1, (op), expr env e2)
  | Unary (op, e) -> A.Unary (expr env e, (op))
  | Prefix  (op, e) -> A.Infix (expr env e, (op))
  | Postfix (e, op) -> A.Postfix (expr env e, (op))

  | Assign (e1, op, e2) ->
      A.Assign ((op), expr env e1, expr env e2)
  | Sequence (e1, _, e2) ->
      A.Sequence (expr env e1, expr env e2)
  | CondExpr (e1, _, e2opt, _, e3) ->
      A.CondExpr (expr env e1,
                  (match e2opt with
                   | Some e2 -> expr env e2
                   | None ->
                       debug (Expr e); raise Todo
                  ),
                  expr env e3)
  | Call (e, (t1, args, t2)) ->
      A.Call (expr env e,
              (t1, Common.map_filter (argument env) (args), t2))

  | SizeOf (tok, Left e) ->
      A.SizeOf(tok, Left (expr env e))
  | SizeOf (tok, Right (_, ft, _)) ->
      A.SizeOf(tok, Right (full_type env ft))
  | GccConstructor ((_, ft, _), xs) ->
      A.GccConstructor (full_type env ft,
                        initialiser env (InitList xs))

  | ConstructedObject (_, _) ->
      logger#error "BUG PARSING LOCAL DECL PROBABLY";
      debug (Expr e);
      raise CplusplusConstruct

  | Throw _|Delete (_, _, _, _)|New (_, _, _, _, _)
  | CplusplusCast (_, _, _)
  | IdSpecial (This, _)
  | DotStarAccess (_, _, _)
  | TypeId (_, _)
  | ParamPackExpansion _
  | Lambda _
    ->
      debug (Expr e); raise CplusplusConstruct

  | StatementExpr _
  | ExprTodo _
  | IdSpecial _
    ->
      debug (Expr e); raise Todo

  | ParenExpr (_, e, _) -> expr env e
  (* sgrep-ext: *)
  | TypedMetavar (id, t) -> A.TypedMetavar (id, full_type env t)

and constant _env x =
  match x with
  | Int (s, ii) -> A.Int (s, ii)
  | Float (s, ii) -> A.Float (s, ii)
  | Char (s, ii) -> A.Char (s, ii)
  | String (s, ii) -> A.String (s, ii)
  | Nullptr ii -> A.Null ii
  | Bool x -> A.Bool x
  | MultiString iis -> A.String ("TODO", iis |> List.hd |> snd)

and argument env x =
  match x with
  | Arg e -> Some (A.Arg (expr env e))
  (* TODO! can't just skip it ... *)
  | ArgType _  | ArgAction _ ->
      logger#error "type argument, maybe wrong typedef inference!";
      debug (Argument x);
      None
  | ArgInits _ -> raise CplusplusConstruct

(* ---------------------------------------------------------------------- *)
(* Type *)
(* ---------------------------------------------------------------------- *)

and primitive_type _env (x, t) =
  let s  =
    match x with
    | TVoid -> "void"
    | TBool -> "bool"
    | TChar -> "char"
    | TInt -> "int"
    | TFloat -> "float"
    | TDouble -> "Double"
  in
  s, t

and sized_type (x, _) =
  match x with
  | TSigned -> "signed"
  | TUnsigned -> "unsigned"
  | TShort -> "short"
  | TLong -> "long"

and full_type env x =
  let (_qu, (t)) = x in
  match t with
  | TypeTodo _ -> raise CplusplusConstruct
  | TPointer (tok, t, _) -> A.TPointer (tok, full_type env t)
  | TPrimitive (x, t) ->
      A.TBase (primitive_type env (x, t))
  | TSized (xs, baseopt) ->
      let ys = xs |> List.map sized_type in
      let ii = xs |> List.map snd in
      let last_s, last_i =
        match baseopt with
        | None -> "", []
        | Some t ->
            (match unwrap_typeC t with
             | TPrimitive (x, t) ->
                 let (s, t) = primitive_type env (x, t) in
                 s, [t]
             | _ -> raise CplusplusConstruct
            )
      in
      let iis = ii @ last_i in
      let ii =
        match iis with
        | [] -> raise Impossible
        | x::xs -> PI.combine_infos x xs
      in
      A.TBase ((ys @ [ last_s ]) |> String.concat "_", ii)

  | TFunction ft -> A.TFunction (function_type env ft)
  | TArray ((_, eopt, _), ft) ->
      A.TArray (Option.map (expr env) eopt, full_type env ft)
  | TypeName n -> A.TTypeName (name env n)

  | ClassName ((kind, _), n) ->
      A.TStructName (struct_kind env kind, name env n)
  | ClassDef (name_opt, def) ->
      (match def with
         { c_kind = (kind, tok);
           c_inherit = _inh;
           c_members = (t1, xs, t2);
         } ->
           let name =
             match name_opt with
             | None ->
                 incr cnt;
                 let s = gensym_struct !cnt in
                 (s, tok)
             | Some n -> name env n
           in
           let def' = { A.
                        s_name = name;
                        s_kind = struct_kind env kind;
                        s_flds = (t1, class_members_sequencable env xs |> List.flatten, t2);
                      }
           in
           env.struct_defs_toadd <- def' :: env.struct_defs_toadd;
           A.TStructName (struct_kind env kind, name)
      )

  | EnumName (_tok, n) -> A.TEnumName (name env n)
  | EnumDef ({enum_kind = tok; enum_name = name_opt; enum_body = xs}) ->
      let name =
        match name_opt with
        | None ->
            incr cnt;
            let s = gensym_enum !cnt in
            (s, tok)
        | Some n -> name env n
      in
      let xs' =
        xs |> unparen |> List.map (fun eelem ->
          let (name, e_opt) = eelem.e_name, eelem.e_val in
          name,
          match e_opt with
          | None -> None
          | Some (_tok, e) -> Some (expr env e)
        )
      in
      let def = { A.e_name = name; e_consts = xs' } in
      env.enum_defs_toadd <- def :: env.enum_defs_toadd;
      A.TEnumName name

  | TypeOf (_, _) ->
      debug (Type x); raise Todo
  | TypenameKwd (_, _) | TReference _ | TAuto _ | TRefRef _ ->
      debug (Type x); raise CplusplusConstruct

  | ParenType (_, t, _) -> full_type env t

(* ---------------------------------------------------------------------- *)
(* structure *)
(* ---------------------------------------------------------------------- *)
and class_member env x =
  match x with
  | F (DeclList (xs, _)) ->
      xs |> List.map (fieldkind env)
  | ( QualifiedIdInClass (_, _)| Access (_, _) | Friend _ ) ->
      debug (ClassMember x); raise CplusplusConstruct
  | F (EmptyDef _) -> []
  | F _ -> debug (ClassMember x); raise Todo


and class_members_sequencable env xs =
  ifdef_skipper xs (function CppIfdef x -> Some x | _ -> None)
  |> List.map (class_member_sequencable env)

and class_member_sequencable env x =
  match x with
  | X x -> class_member env x
  | CppDirective dir -> debug (Cpp dir); raise Todo
  | CppIfdef _ -> raise Impossible
  | (MacroVar (_, _)|MacroDecl (_, _, _, _)) -> raise Todo

and fieldkind env decl =
  match decl with
  | EmptyDecl ft ->  { A.fld_name = None; fld_type = full_type env ft }
  | TypedefDecl (_tk, _ty, _id) ->
      debug (OneDecl decl); raise Todo
  | V ({ name = n; specs = _check_sto_emptyTODO}, { v_init = _TODOcheckNone; v_type = ft }) ->
      { A.
        fld_name = Some (name env n);
        fld_type = full_type env ft;
      }
  | StructuredBinding _ -> raise CplusplusConstruct
  | BitField (name_opt, _tok, ft, e) ->
      let _TODO = expr env e in
      { A.
        fld_name = name_opt;
        fld_type = full_type env ft;
      }

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

and name _env x =
  match x with
  | (None, [], IdIdent name) -> name
  | _ -> debug (Name x); raise CplusplusConstruct

and struct_kind _env = function
  | Struct -> A.Struct
  | Union -> A.Union
  | Class -> raise CplusplusConstruct

(*****************************************************************************)
(* Other entry points *)
(*****************************************************************************)
let any any =
  let env = empty_env () in

  match any with
  | Expr x -> A.Expr (expr env x)
  | Stmt x -> A.Stmt (stmt env x)
  | Stmts xs -> A.Stmts (List.map (stmt env) xs)
  | Toplevel x ->
      (match toplevel env x with
       | [x] -> A.Stmt x
       | xs -> A.Stmts xs
      )
  | Toplevels xs -> A.Stmts (List.map (toplevel env) xs |> List.flatten)

  | _
    -> failwith "Ast_c_simple_build.any: only Expr/Stmt/Stmts handled"
