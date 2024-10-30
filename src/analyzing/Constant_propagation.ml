(* Yoann Padioleau
 * Iago Abal
 *
 * Copyright (C) 2020-2022 Semgrep Inc.
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
open AST_generic
module Log = Log_analyzing.Log
module G = AST_generic
module H = AST_generic_helpers
module Eval = Eval_generic_partial

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Two-pass constant propagation.
 *
 * 1. First pass is a basic flow-insensitive analysis but, crucially, it
 * considers global and class-level constants.
 * 2. Second pass is flow-sensitive, but intraprocedural and ignores the
 * global scope, it uses and refines the outcome of the first pass.
 *
 * This is mainly to provide advanced features to semgrep such as the
 * constant propagation of literals.
 *
 * Partial evaluation for Generic is provided by core/ast/Normalize_generic.ml,
 * we may not need it when we annotate every expr with constant/svalue info.
 *
 * Right now we just propagate constants when we are sure* that it is a constant
 * because:
 *  - the variable declaration use the 'const' keyword in Javascript/Go/...
 *  - the field declaration use the 'final' keyword in Java
 *  - we do a very basic const analysis where we check the variable
 *    is never assigned more than once.
 *
 * * We cannot be 100% sure e.g. due to aliasing.
 *
 * history:
 * - ver1: this used to be in Naming_AST.ml but better to split, even though
 * things will be slightly slower because we will visit the same file
 * twice.
 * - ver2: added second flow-sensitive constant propagation pass.
 * - ver3: do not assign constant values to labels; in x = E, x is just a label
 * (a "ref") and denotes a memory location rather than the value stored in it.
 *
 * TODO: Remove duplication between the Generic-based and IL-based passes,
 * ideally we should have a single pass, the IL-based.
 *)

let tags = Log_analyzing.svalue_tag

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type lr_stats = {
  (* note that a VarDef defining the value will count as 1 *)
  lvalue : int ref;
  rvalue : int ref;
}

let default_lr_stats () = { lvalue = ref 0; rvalue = ref 0 }

type class_stats = { mutable num_constructors : int }

type stats = {
  var_stats : (Eval.var, lr_stats) Hashtbl.t;
  class_stats : (string, class_stats) Hashtbl.t;
}

let new_stats () =
  { var_stats = Hashtbl.create 100; class_stats = Hashtbl.create 1 }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( let/ ) o f = Option.iter f o

let is_private attr =
  match attr with
  | KeywordAttr (Private, _) -> true
  | _ -> false

(* TODO: incomplete, e.g. Record is not handled *)
let rec lvars_in_lhs expr =
  match expr.e with
  | N (Id (id, { id_resolved = { contents = Some (_kind, sid) }; _ }))
  | DotAccess
      ( { e = IdSpecial (This, _); _ },
        _,
        FN (Id (id, { id_resolved = { contents = Some (_kind, sid) }; _ })) ) ->
      [ (id, sid) ]
  | Container ((Tuple | Array), (_, es, _)) -> List.concat_map lvars_in_lhs es
  | __else__ -> []

let no_cycles_in_sym_prop sid exp =
  let for_all_sid : (G.sid -> bool) -> G.any -> bool =
    (* Check that all sid's satisfy a given condition. We use refs so that
     * we can have a single visitor for all calls, given that the old
     * `mk_visitor` was kind of expensive, and constructing a visitor object
     * may be as well. *)
    let ff = ref (fun _ -> assert false) in
    let ok = ref true in
    let vout =
      object
        inherit [_] G.iter

        method! visit_resolved_name _env (_, sid) =
          ok := !ok && !ff sid;
          if not !ok then raise Exit
      end
    in
    fun f ast ->
      ff := f;
      ok := true;
      try
        vout#visit_any () ast;
        !ok
      with
      | Exit -> false
  in
  for_all_sid (fun sid' -> sid' <> sid) (E exp)

(*****************************************************************************)
(* Environment Helpers *)
(*****************************************************************************)

let is_class_field env = function
  | EnclosedVar (* OSS *)
  | GlobalName _ (* Pro *) ->
      Eval.is_lang env Lang.Java
  | Global
  | LocalVar
  | Parameter
  | ImportedEntity _
  | ImportedModule _
  | TypeName
  | Macro
  | EnumConstant ->
      false

let is_resolved_name _kind sid = not (SId.is_unsafe_default sid)

let add_constant_env ident (sid, svalue) (env : Eval.env) =
  match svalue with
  | Lit _
  | Cst _
  | Sym _ ->
      Log.debug (fun m ->
          m ~tags "adding constant in env %s" (H.str_of_ident ident));
      Hashtbl.add env.constants (H.str_of_ident ident, sid) svalue
  | NotCst -> ()

let is_assigned_just_once stats var =
  let id_str, sid = var in
  match Hashtbl.find_opt stats var with
  | Some stats -> !(stats.lvalue) = 1
  | None ->
      Log.debug (fun m ->
          m ~tags "No stats for (%s,%s)" id_str (G.SId.show sid));
      false

let incr_num_constructors stats cid =
  let stats_cid =
    let cstr, _tok = cid in
    try Hashtbl.find stats.class_stats cstr with
    | Not_found ->
        let stats_cid = { num_constructors = 0 } in
        Hashtbl.add stats.class_stats cstr stats_cid;
        stats_cid
  in
  stats_cid.num_constructors <- stats_cid.num_constructors + 1

let has_just_one_constructor stats cstr =
  match Hashtbl.find_opt stats cstr with
  | Some stats -> stats.num_constructors = 1
  | None ->
      Log.debug (fun m -> m ~tags "No stats for %s" cstr);
      false

let constant_propagation_and_evaluate_literal ?lang =
  let env = Eval.default_env lang in
  Eval.eval env

(*****************************************************************************)
(* Poor man''s const analysis *)
(*****************************************************************************)
(* This is mostly useful for languages without a const keyword (e.g., Python).
 *
 * Note that this may be incomplete and buggy. Worst case we do a
 * constant propagation on a variable containing a literal that may
 * have its content changed; not a big-deal in semgrep context actually.
 *
 * TODO: do it also for Java private fields; if they are not used in an
 * lvalue in this class, they can't be accessed from anywhere else so it's
 * safe to do the const propagation.
 *)
let stats_of_prog prog : stats =
  let stats = new_stats () in
  let get_stat_or_create var h =
    try Hashtbl.find h var with
    | Not_found ->
        let stat = default_lr_stats () in
        Hashtbl.add h var stat;
        stat
  in

  let visitor =
    object (_self : 'self)
      inherit [_] Iter_with_context.iter_with_context as super

      method! with_context_visit_definition (env, ctx) x =
        (* We need to override 'with_context_visit_definition' instead of
         * 'visit_definition' because we want to know here whether 'FuncDef'
         * is a constructor. *)
        (match x with
        | _, FuncDef _ when ctx.in_constructor -> (
            match ctx.in_class with
            | None ->
                Log.warn (fun m ->
                    m "stats_of_prog: in constructor but not in class");
                ()
            | Some cid -> incr_num_constructors env cid)
        | ( {
              name =
                EN
                  (Id
                    (id, { id_resolved = { contents = Some (_kind, sid) }; _ }));
              _;
            },
            VarDef { vinit = Some _e; _ } ) ->
            let var = (H.str_of_ident id, sid) in
            let stat = get_stat_or_create var env.var_stats in
            incr stat.lvalue
        | _ -> ());
        super#with_context_visit_definition (env, ctx) x

      (* TODO: An `ExprStmt` method call (probably returning 'void') should count as a
       * potential assignment too... E.g. in Ruby `a.concat(b)` is going to update `a`. *)
      method! visit_stmt (env, ctx) x =
        (match x.s with
        | For
            ( _,
              ForEach
                ( PatId
                    (id, { id_resolved = { contents = Some (_kind, sid) }; _ }),
                  _,
                  _ ),
              _ ) ->
            let var = (H.str_of_ident id, sid) in
            let stat = get_stat_or_create var env.var_stats in
            incr stat.lvalue
        | _ -> ());
        super#visit_stmt (env, ctx) x

      method! visit_expr (env, ctx) x =
        (match x.e with
        | Assign (* v = ... *) (lhs, _, _e2)
        | AssignOp (* v += ... *) (lhs, _, _e2) ->
            (* TODO: What if there is an asignment inside the `lhs` ? *)
            lvars_in_lhs lhs
            |> List.iter (fun (id, sid) ->
                   let var = (H.str_of_ident id, sid) in
                   let stat = get_stat_or_create var env.var_stats in
                   incr stat.lvalue;
                   match x.e with
                   | AssignOp _ -> incr stat.rvalue
                   | _ -> ())
        | Call
            ( { e = IdSpecial (IncrDecr _, _); _ },
              ( _,
                [
                  Arg
                    {
                      e =
                        N
                          (Id
                            ( id,
                              {
                                id_resolved = { contents = Some (_kind, sid) };
                                _;
                              } ));
                      _;
                    };
                ],
                _ ) ) ->
            let var = (H.str_of_ident id, sid) in
            let stat = get_stat_or_create var env.var_stats in
            incr stat.lvalue
        | N (Id (id, { id_resolved = { contents = Some (_kind, sid) }; _ }))
          when not ctx.in_lvalue ->
            let var = (H.str_of_ident id, sid) in
            let stat = get_stat_or_create var env.var_stats in
            incr stat.rvalue
        | _ -> ());
        super#visit_expr (env, ctx) x
    end
  in
  visitor#visit_program (stats, Iter_with_context.initial_context) prog;
  stats

(*****************************************************************************)
(* Terraform hardcoded semantic *)
(*****************************************************************************)
(* ugly: In Terraform/HCL, locals and variables are introduced in a weird way
 * as in 'locals { foo = 1 }' or 'variable "foo" { default = 1 }'. They
 * are also used in weird way, as in 'local.foo' or 'var.foo'.
 *
 * We could modify Parse_hcl_tree_sitter.ml to transform those blocks
 * in VarDef, and the dotted access to 'local' and 'var' in direct
 * access, but some people may want to write patterns to match explicitely
 * those blocks or dotted access. Thus, it is maybe better to keep the code
 * as is but perform extra work just for Terraform in the constant
 * propagation analysis.
 * alt: modify Parse_hcl_tree_sitter.ml
 *
 * For more information on terraform locals and variables semantic
 * see https://www.terraform.io/language/values
 *)

let (terraform_stmt_to_vardefs : item -> (ident * expr) list) =
 fun st ->
  match st.s with
  (* coupling: reverse of Parse_hcl_tree_sitter.map_block *)
  (* ex: locals { foo = 1, bar = 2 } *)
  | ExprStmt
      ( {
          e =
            Call
              ( { e = N (Id (("locals", _), _)); _ },
                (_, [ Arg { e = Record (_, xs, _); _ } ], _) );
          _;
        },
        _ ) ->
      xs
      |> List_.filter_map (function
           | F
               {
                 s =
                   DefStmt
                     ( {
                         name = EN (Id ((str, tk), _idinfo));
                         attrs = [];
                         tparams = None;
                       },
                       VarDef { vinit = Some v; vtype = None; vtok = _ } );
                 _;
               } ->
               Some (("local." ^ str, tk), v)
           | _ -> None)
  (* ex: variable "foo" { ... default = 1 } *)
  | ExprStmt
      ( {
          e =
            Call
              ( { e = N (Id (("variable", _), _)); _ },
                ( _,
                  [
                    Arg { e = L (String (_, id, _)); _ };
                    Arg { e = Record (_, xs, _); _ };
                  ],
                  _ ) );
          _;
        },
        _ ) ->
      xs
      |> List_.filter_map (function
           | F
               {
                 s =
                   DefStmt
                     ( {
                         name = EN (Id (("default", _tk), _idinfo));
                         attrs = [];
                         tparams = None;
                       },
                       VarDef { vinit = Some v; vtype = None; vtok = _ } );
                 _;
               } ->
               let str, tk = id in
               Some (("var." ^ str, tk), v)
           | _ -> None)
  | _ -> []

(* the sid does not matter here, there is no nested scope in terraform,
 * no shadowing, and we prefix with var. and local. so there is no
 * ambiguity and name clash.
 *)
let terraform_sid = SId.unsafe_default

let add_special_constants env lang prog =
  if lang = Lang.Terraform then
    let vars = prog |> List.concat_map terraform_stmt_to_vardefs in
    vars
    |> List.iter (fun (id, v) ->
           match v.e with
           | L literal ->
               Log.debug (fun m ->
                   m ~tags "adding special terraform constant for %s" (fst id));
               add_constant_env id (terraform_sid, Lit literal) env
           | _ -> ())

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

type propagate_basic_visitor_funcs = {
  visit_definition :
    Eval.env * Iter_with_context.context -> AST_generic.definition -> unit;
}

let hook_propagate_basic_visitor : propagate_basic_visitor_funcs option ref =
  ref None

(* !Note that this assumes Naming_AST.resolve has been called before! *)
let propagate_basic lang prog =
  Log.debug (fun m -> m ~tags "Constant_propagation.propagate_basic program");
  let (env : Eval.env) = Eval.default_env (Some lang) in

  (* right now this is used only for Terraform *)
  add_special_constants env lang prog;

  (* step1: first pass const analysis for languages without 'const/final' *)
  let stats = stats_of_prog prog in

  (* step2: second pass where we actually propagate when we can *)
  let visitor =
    object (_self : 'self)
      inherit [_] Iter_with_context.iter_with_context as super

      (* the defs *)
      method! visit_definition ((env : Eval.env), ctx) x =
        match x with
        | ( {
              name =
                EN
                  (Id
                    (id, { id_resolved = { contents = Some (_kind, sid) }; _ }));
              attrs;
              _;
            },
            VarDef { vinit = None; _ } ) ->
            if is_assigned_just_once stats.var_stats (H.str_of_ident id, sid)
            then
              (* This is a potential constant but it's defined later on, so we store
               * it's attributes so check them later, e.g. to know whether it has
               * `private` visibility. An example of this is a class field that
               * is initialized in the constructor or in a `static` block. *)
              Hashtbl.replace env.attributes (fst id, sid) attrs;
            super#visit_definition (env, ctx) x
        | ( {
              name =
                EN
                  (Id
                    ( id,
                      {
                        id_resolved = { contents = Some (_kind, sid) };
                        id_flags;
                        _;
                      } ));
              attrs;
              _;
            },
            VarDef { vinit = Some e; _ } )
        (* note that some languages such as Python do not have VarDef.
         * todo? should add those somewhere instead of in_lvalue detection?*) ->
            let assigned_just_once =
              is_assigned_just_once stats.var_stats (H.str_of_ident id, sid)
            in
            if
              H.has_keyword_attr Const attrs
              || H.has_keyword_attr Final attrs
              || (assigned_just_once && Eval.is_js env)
              || assigned_just_once && Eval.is_lang env Lang.Java
                 && List.exists is_private attrs
            then (
              id_flags := IdFlags.set_final !id_flags;
              match (Eval.eval env e, e.e) with
              (* When the name already has an svalue computed, just use
               * that. DeepSemgrep assigns svalues sometimes in its naming
               * phase. *)
              | Some svalue, _ -> add_constant_env id (sid, svalue) env
              | None, L literal -> add_constant_env id (sid, Lit literal) env
              (* For any other symbolic expression, it is OK to propagate it symbolically so long as
                 the lvalue is only assigned to once.
                 Although we may propagate expressions with identifiers in them, those identifiers
                 will simply not have an `svalue` if they are non-propagated as well.
              *)
              | None, _
                when Dataflow_svalue.is_symbolic_expr e
                     && no_cycles_in_sym_prop sid e ->
                  add_constant_env id (sid, Sym e) env
              | None, _ -> ());
            super#visit_definition (env, ctx) x
        | _ ->
            !hook_propagate_basic_visitor
            |> Option.iter (fun v -> v.visit_definition (env, ctx) x);
            super#visit_definition (env, ctx) x

      (* THINK: Should we just visit every single name ? *)
      method! visit_NamedAttr (env, ctx) tok name args =
        (match name with
        | Id (id, id_info) ->
            let/ svalue = Eval.find_id env id id_info in
            Dataflow_svalue.set_svalue_ref id_info svalue
        | __else__ -> ());
        super#visit_NamedAttr (env, ctx) tok name args

      (* the uses (and also defs for Python Assign) *)
      method! visit_expr (env, ctx) x =
        match x.e with
        | N (Id (id, id_info))
        | DotAccess
            ({ e = IdSpecial ((This | Self), _); _ }, _, FN (Id (id, id_info)))
          when not ctx.in_lvalue ->
            let/ svalue = Eval.find_id env id id_info in
            Dataflow_svalue.set_svalue_ref id_info svalue
        (* ugly: dockerfile specific *)
        | Call
            ( { e = N (Id (("!dockerfile_expand!", _), _)); _ },
              (_, [ Arg { e = N (Id (id, id_info)); _ } ], _) )
          when not ctx.in_lvalue ->
            let/ svalue = Eval.find_id env id id_info in
            Dataflow_svalue.set_svalue_ref id_info svalue
        (* ugly: terraform specific.
         * coupling: with eval() above
         *)
        | DotAccess
            ( { e = N (Id (((("local" | "var") as prefix), _), _)); _ },
              _,
              FN (Id ((str, _tk), id_info)) )
          when lang = Lang.Terraform && not ctx.in_lvalue ->
            let var = (prefix ^ "." ^ str, terraform_sid) in
            let/ svalue = Hashtbl.find_opt env.constants var in
            Dataflow_svalue.set_svalue_ref id_info svalue
        | Assign
            (* Assign that is really a hidden VarDef (e.g., in Python) *)
            ( {
                e =
                  ( N
                      (Id
                        ( id,
                          {
                            id_resolved = { contents = Some (kind, sid) };
                            id_flags;
                            _;
                          } ))
                  | DotAccess
                      ( { e = IdSpecial ((This | Self), _); _ },
                        _,
                        FN
                          (Id
                            ( id,
                              {
                                id_resolved = { contents = Some (kind, sid) };
                                id_flags;
                                _;
                              } )) ) );
                _;
              },
              _,
              rexp ) ->
            let opt_svalue = Eval.eval env rexp in
            let is_private_class_field =
              match Hashtbl.find_opt env.attributes (fst id, sid) with
              | None -> false
              | Some attrs ->
                  List.exists is_private attrs && is_class_field env kind
            in
            let in_unique_constructor =
              match ctx.in_class with
              | None -> false
              | Some cid ->
                  ctx.in_constructor
                  && has_just_one_constructor stats.class_stats (fst cid)
            in
            if
              is_assigned_just_once stats.var_stats (H.str_of_ident id, sid)
              (* restricted to prevent unexpected const-prop FPs *)
              && ((Eval.is_lang env Lang.Python
                  || Eval.is_lang env Lang.Ruby || Eval.is_lang env Lang.Php
                  || Eval.is_js env)
                  && H.name_is_global kind
                 (* TODO: Add other Java-like OO languages, maybe Apex and C# ? *)
                 || Eval.is_lang env Lang.Java && is_private_class_field
                    && (ctx.in_static_block || in_unique_constructor))
              && is_resolved_name kind sid
            then (
              id_flags := IdFlags.set_final !id_flags;
              match opt_svalue with
              | Some svalue -> add_constant_env id (sid, svalue) env
              | None ->
                  if
                    Dataflow_svalue.is_symbolic_expr rexp
                    && no_cycles_in_sym_prop sid rexp
                  then add_constant_env id (sid, Sym rexp) env;
                  ());
            super#visit_expr (env, ctx) rexp
        | __else__ -> super#visit_expr (env, ctx) x
    end
  in
  visitor#visit_program (env, Iter_with_context.initial_context) prog;
  ()
[@@profiling] [@@trace_trace]

let propagate_dataflow_one_function lang fun_cfg =
  (* Exposed to help DeepSemgrep *)
  let mapping = Dataflow_svalue.fixpoint lang fun_cfg in
  Dataflow_svalue.update_svalue fun_cfg.cfg mapping

let propagate_dataflow lang ast =
  Log.debug (fun m -> m ~tags "Constant_propagation.propagate_dataflow program");
  match lang with
  | Lang.Dockerfile ->
      (* Dockerfile has no functions. The whole file is just a single scope *)
      let xs =
        AST_to_IL.stmt lang (G.Block (Tok.unsafe_fake_bracket ast) |> G.s)
      in
      (* Top-level function. No need to use CFG_build.cfg_of_gfdef here. *)
      let cfg, lambdas = CFG_build.cfg_of_stmts xs in
      propagate_dataflow_one_function lang IL.{ params = []; cfg; lambdas }
  | _ ->
      ast
      |> Visit_function_defs.visit (fun _ent fdef ->
             let fun_cfg = CFG_build.cfg_of_gfdef lang fdef in
             (* when/ pattern when is not constant propagation but is related in the sense
              * that it also finds extra info by analyzing the cfg. we are reusing the cfg
              * created here to annotate facts for the pattern when feature.
              *
              * TODO: refactor this code if we don't incorporate when into svalue analysis.
              *)
             (match !Dataflow_when.hook_annotate_facts with
             | None -> ()
             | Some annotate_facts -> annotate_facts fun_cfg.cfg);
             propagate_dataflow_one_function lang fun_cfg);

      (* We consider the top-level function the interior of a degenerate function,
         and simply run constant propagation on that.

         Since we don't traverse into each function body recursively, we shouldn't
         duplicate any work.
      *)
      let xs = AST_to_IL.stmt lang (G.stmt1 ast) in
      (* Top-level function. No need to use CFG_build.cfg_of_gfdef here. *)
      let cfg, lambdas = CFG_build.cfg_of_stmts xs in
      propagate_dataflow_one_function lang IL.{ params = []; cfg; lambdas }
[@@trace_trace]
