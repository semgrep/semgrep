(* Yoann Padioleau
 *
 * Copyright (C) 2011, 2012, 2013 Facebook
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

open Cst_php

module Ast = Cst_php
module V = Visitor_php
module E = Entity_code
module CG = Callgraph_php2
module P = Prolog_code
module PI = Parse_info
module T = Parser_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module makes it possible to ask questions on the structure of
 * a PHP codebase, for instance: "What are all the children of class Foo?".
 * It is inspired by a similar tool for java called JQuery
 * (http://jquery.cs.ubc.ca/).
 *
 * history:
 *  - basic defs (kinds, at)
 *  - inheritance tree
 *  - basic callgraph
 *  - basic datagraph
 *  - include/require (and possibly desugared wrappers like require_module())
 *  - precise callgraph, using julien's abstract interpreter (was called
 *    previously pathup/pathdown)
 *  - types (when the code is annotated)
 *
 * todo:
 *  - get rid of P.Misc, generate precise facts, see graph_code_prolog.ml
 *  - precise datagraph
 *  - refs
 *
 * For more information look at h_program-lang/prolog_code.pl
 * and its many predicates.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let name_of_node = function
  | CG.File s -> spf "'__TOPSTMT__%s'" s
  | CG.Function s -> spf "'%s'" s
  | CG.Method (s1, s2) -> spf "('%s','%s')" s1 s2
  | CG.FakeRoot -> "'__FAKE_ROOT__'"

let string_of_modifier = function
  | Public    -> "is_public"
  | Private   -> "is_private"
  | Protected -> "is_protected"
  | Static -> "static"  | Abstract -> "abstract" | Final -> "final"
  | Async -> "async"

let rec string_of_hint_type x =
  match x with
  (* todo: figure out a reasonable respresentation for type args in prolog
   * could just represent them as a term, so people could actually use
   * unification over it too, e.g. 'Ent'('Foo'), array(int), etc.
   *)
  | Hint (c, _targsTODO) ->
    (match c with
    | XName [QI (c)] -> Ast.str_of_ident c
    | XName qu ->
      raise (Ast.TodoNamespace (Ast.info_of_qualified_ident qu))
    | Self _ -> "self"
    | Parent _ -> "parent"
    | LateStatic _ -> "")
  | HintArray _ -> "array"
  | HintQuestion (_, t) -> "?" ^ (string_of_hint_type t)
  | HintTuple v1 ->
    let elts = List.map
      (fun x -> string_of_hint_type x)
      (Ast.uncomma (Ast.unparen v1))
    in
    "(" ^ (String.concat ", " elts) ^ ")"
  | HintCallback _ -> "callback"
  | HintShape _ -> "shape"
  | HintTypeConst _ -> "typeconst"
  | HintVariadic _ -> "..."

let string_of_hint_type_opt h =
  match h with
  | Some x -> string_of_hint_type x
  (* todo: maybe we could grab information from hh_server *)
  | None -> "!unknown!"

let read_write in_lvalue =
  if in_lvalue then "write" else "read"

let escape_quote_array_field s =
  Str.global_replace (Str.regexp "[']") "__" s

let add_function_params current def add =
  def.f_params |> Ast.unparen |> Ast.uncomma_dots |> Common.index_list_0 |>
    List.iter (fun (param, i) ->
      add (P.Misc (spf "parameter(%s, %d, '$%s', '%s')"
             current
             i
             (Ast.str_of_dname param.p_name)
             (string_of_hint_type_opt param.p_type)
      ))
    );
  add (P.Misc (spf "return(%s, '%s')" current
        (string_of_hint_type_opt
           (match def.f_return_type with
           | None -> None
           | Some (_colon, _atopt, t) -> Some t
           ))))

(*****************************************************************************)
(* Defs/uses *)
(*****************************************************************************)

(* Yet another use/def ...
 *
 * Factorize code with defs_uses_php.ml?
 * But for defs we want more than just defs, we also want the arity
 * of parameters for instance. And for uses we also want sometimes to
 * process the arguments for instance with require_module, so it's hard
 * to factorize I think. Copy paste is fine sometimes ...
 *
 * Factorize code with graph_code_php.ml?
 * But many things like approximate callgraphs with docall(_, 'bar', method)
 * are not in the graph_code, or parameters, so we need anyway to
 * visit the AST to add those speific predicates.
 *)
let visit ~add readable ast =

  let h = Hashtbl.create 101 in

  let current = ref (spf "'__TOPSTMT__%s'" readable) in
  let in_lvalue_pos = ref false in

  let visitor = V.mk_visitor { V.default_visitor with
    V.ktop = (fun (k, vx) x ->
      match x with
      | FuncDef def ->
        let s = Ast.str_of_ident def.f_name in
        current := spf "'%s'" s;
        Hashtbl.clear h;
        add (P.Kind (P.entity_of_str s, E.Function));
        add (P.At (P.entity_of_str s, readable, PI.line_of_info def.f_tok));

        add (P.Misc (spf "arity(%s, %d)" !current
             (List.length (def.f_params |> Ast.unparen |> Ast.uncomma_dots))));
        add_function_params !current def add;
        def.f_modifiers |> List.iter (fun (m, _) ->
          add (P.Misc (spf "%s(%s)" (string_of_modifier m) !current));
        );

        k x
      | ConstantDef {cst_toks =(tok, _, _); cst_name=id; cst_type=topt; _} ->
        let s = Ast.str_of_ident id in
        current := spf "'%s'" s;
        Hashtbl.clear h;
        add (P.Kind (P.entity_of_str s, E.Constant));
        add (P.At (P.entity_of_str s, readable, PI.line_of_info tok));
        add (P.Type (P.entity_of_str s, (string_of_hint_type_opt topt)));
        k x

      | ClassDef def ->
        let s = Ast.str_of_ident def.c_name in
        current := spf "'%s'" s;
        Hashtbl.clear h;
        let kind_str = Class_php.string_of_class_type def.c_type in
        (* add (P.Kind (P.entity_of_str s, E.Class)); *)
        add (P.Misc (spf "kind('%s', %s)" s kind_str));
        let tok = Ast.info_of_ident def.c_name in
        add (P.At (P.entity_of_str s, readable, PI.line_of_info tok));

        (match def.c_type with
        | ClassAbstract _ -> add (P.Misc (spf "abstract(%s)" !current))
        | ClassFinal _ -> add (P.Misc (spf "final(%s)" !current))
        | ClassAbstractFinal _ ->
          add (P.Misc (spf "final(%s)" !current));
          add (P.Misc (spf "abstract(%s)" !current))
        | ClassRegular _ -> ()
        | Enum _ -> ()
        (* the kind/2 will cover those different cases *)
        | Interface _
        | Trait _ -> ()
        );
        def.c_extends |> Common.do_option (fun (_tok, x) ->
          add (P.Extends (s, (Ast.str_of_class_name x)));
        );
        def.c_implements |> Common.do_option (fun (_tok, interface_list) ->
          interface_list |> Ast.uncomma |> List.iter (fun x ->
          (* could put implements instead? it's not really the same
           * kind of extends. Or have a extends_interface/2? maybe
           * not worth it, just add kind(X, class) when using children/2
           * if you want to restrict your query.
           *)
            (match def.c_type with
            | Interface _ ->
              add (P.Extends (s, Ast.str_of_class_name x));
            | _ ->
              add (P.Implements(s, Ast.str_of_class_name x));
            )
          ));
        def.c_body |> Ast.unbrace |> List.iter (function
        | UseTrait (_tok, names, _rules_or_tok) ->
          names |> Ast.uncomma |> List.iter (fun name ->
            add (P.Mixins (s, Ast.str_of_class_name name))
          )
        | _ -> ()
        );

        def.c_body |> Ast.unbrace |> List.iter (fun class_stmt ->
          match class_stmt with
          | Method def ->

          let s2 = Ast.str_of_ident def.f_name in
          current := spf "('%s', '%s')" s s2;
          Hashtbl.clear h;
          let sfull = (s ^ "." ^ s2) in
          add (P.Kind (P.entity_of_str sfull, E.Method));
          add (P.At (P.entity_of_str sfull, readable, PI.line_of_info def.f_tok));

          add (P.Misc (spf "arity(%s, %d)" !current
             (List.length (def.f_params |> Ast.unparen |> Ast.uncomma_dots))));
          def.f_modifiers |> List.iter (fun (m, _) ->
            add (P.Misc (spf "%s(%s)" (string_of_modifier m) !current));
          );
          add_function_params !current def add;

          vx (ClassStmt class_stmt);

        | ClassConstants (_abstok, tok, _typopt, xs, _sc) ->
          xs |> Ast.uncomma |> List.iter (fun (id, sc_opt) ->
            let s2 = Ast.str_of_ident id in
            current := spf "('%s', '%s')" s s2;
            Hashtbl.clear h;
            let sfull = (s ^ "." ^ s2) in
            add (P.Kind (P.entity_of_str sfull, E.ClassConstant));
            add (P.At (P.entity_of_str sfull, readable, PI.line_of_info tok));

            sc_opt |> Common.do_option (fun (_, e) -> vx (Expr e))
          )
        | ClassVariables (ms, topt, xs, _sc) ->

          xs |> Ast.uncomma |> List.iter (fun classvar ->
            let (dname, sc_opt) = classvar in
            let s2 = Ast.str_of_dname dname in

            (* old: I used to do something different for Field amd remove
             * the $ because in use-mode but we don't use the $ anymore.
             * now def don't have a $ too, and can be xhp attribute too.
             *)
            current := spf "('%s', '%s')" s s2;
            let sfull = (s ^ "." ^ s2) in
            Hashtbl.clear h;
            add (P.Kind (P.entity_of_str sfull, E.Field));
            add (P.At (P.entity_of_str sfull, readable, PI.line_of_info tok));

            (match ms with
            | NoModifiers _ -> ()
            | VModifiers ms ->
              ms |> List.iter (fun (m, _) ->
                add (P.Misc (spf "%s(%s)" (string_of_modifier m) !current))
              );
            );
            add (P.Type (P.entity_of_str sfull, string_of_hint_type_opt topt));
            sc_opt |> Common.do_option (fun (_, e) -> vx (Expr e))
          )

        | XhpDecl decl ->
          (match decl with
          | XhpAttributesDecl (tok, xs, _sc) ->
            xs |> Ast.uncomma |> List.iter (function
            | XhpAttrDecl (_t, (name, _tok), _affect_opt, _tok_opt) ->
              let s2 = name in
              current := spf "('%s', '%s')" s s2;
              Hashtbl.clear h;
              let sfull = (s ^ "." ^ s2) in
              let kind = E.Field in
              add (P.Kind (P.entity_of_str sfull, kind));
              add (P.At (P.entity_of_str sfull, readable, PI.line_of_info tok));

            | XhpAttrInherit _ -> ()
            )

          (* less: add some use edge? *)
          | XhpChildrenDecl _ | XhpCategoriesDecl _ ->
            ()
          )

        | UseTrait _ -> ()
        | ClassType _ -> ()
        | TraitConstraint _ -> ()
        )
      | _ -> k x

    );

    V.kexpr = (fun (k, vx) x ->
      match x with

      (* todo: need to handle pass by ref too so set in_lvalue_pos
       * for the right parameter. So need an entity_finder?
       *)
      | Call (Id callname, args) ->
          let str = Cst_php.str_of_name callname in
          let args = args |> Ast.unparen |> Ast.uncomma in
          (match str, args with
          (* Many entities (functions, classes) in PHP are passed as
           * string parameters to some higher-order functions. We normally
           * don't index in the Prolog database the arguments to functions, but
           * certain function calls are really disguised calls
           * to new, hence the special cases below.
           *
           * todo: we should automatically extract the list of all
           * higher-order functions.
           *)
          | ("newv" | "DT"), Arg ((Sc (C (String (str2,_)))))::_rest ->
              add (P.Misc (spf "docall(%s, ('%s','%s'), special)"
                     !current str str2))

          (* could be encoded as a docall(...,special) too *)
          | "require_module", [Arg ((Sc (C (String (str,_)))))] ->
              add (P.Misc (spf "require_module('%s', '%s')"
                     readable str))

          | _ -> ()
          );
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            add (P.Misc (spf "docall(%s, '%s', function)" !current str))
          end;
          k x

      | ArrayGet (_lval, (_, Some((Sc(C(String((fld, _)))))), _)) ->
          let str = escape_quote_array_field fld in
          (* use a different namespace than func? *)
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            add (P.Misc (spf "use(%s, '%s', array, %s)"
                  !current str (read_write !in_lvalue_pos)))
          end;
          k x

      | Call (ObjGet (e, _, Id name), args)
      | Call (ClassGet (e, _, Id name), args)
        ->
          let str = Cst_php.str_of_name name in
          (* use a different namespace than func? *)
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            (* todo: imprecise, need julien's precise callgraph *)
            add (P.Misc (spf "docall(%s, '%s', method)" !current str))
          end;

          (match x with
          | Call (ClassGet(qu,_tok, Id _name), _args) ->
              (match qu with
              | Id (name2) ->
                (match name2 with
                | XName _name ->
                  add (P.Misc (spf "docall(%s, ('%s','%s'), method)"
                           !current (Cst_php.str_of_name name2) str))
                (* this should have been desugared while building the
                 * code database, except for traits code ...
                 *)
                | Self _| Parent _
                (* can't do much ... *)
                | LateStatic _ -> ()
                )
              | _ -> ()
              )
          | Call (ObjGet (_, _, Id _name), _args) ->
            ()
          | _ -> raise Impossible
          );

          (* don't recurse on x, because we don't want to process the
           * ObjGet and ClassGet
           *)
          vx (Expr e);
          vx (Arguments (Ast.unparen args))


      (* the context should be anything except Call *)
      | ClassGet(qu, _tok, Id cstname) ->
         (match qu with
         | Id name ->
           (match name with
           | XName[QI (classname)] ->
             add (P.Misc (spf "use(%s, ('%s','%s'), constant, read)"
                   !current (Cst_php.str_of_ident classname)
                   (Cst_php.str_of_name cstname)))
             (* this should have been desugared while building the
              * code database, except for traits code ...
              *)
          | XName qu ->
            raise (Ast.TodoNamespace (Ast.info_of_qualified_ident qu))
           | Self _| Parent _
           (* can't do much ... *)
           | LateStatic _ -> ()
           )
         | _ -> ()
         );
        k x

      (* the context should be anything except Call *)
      | ObjGet (_, _tok, Id name) ->
          let str = Cst_php.str_of_name name in
          (* use a different namespace than func? *)
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            add (P.Misc (spf "use(%s, '%s', field, %s)"
                   !current str (read_write !in_lvalue_pos)))
          end;
          k x


      (* todo: enough? need to handle pass by ref too here *)
      | Assign (lval, _, e)
      | AssignOp(lval, _, e)
        ->
          Common.save_excursion in_lvalue_pos true (fun () ->
            vx (Expr lval)
          );
          vx (Expr e);


      | New (_, classref, _args)
      | AssignNew (_, _, _, _, classref, _args) ->

        (match classref with
        | Id name ->(* TODO: currently ignoring type args *)
          (match name with
          | XName [QI name] ->
            let str = Cst_php.str_of_ident name in
          (* use a different namespace than func? *)
            if not (Hashtbl.mem h str)
            then begin
              Hashtbl.replace h str true;
              add (P.Misc (spf "docall(%s, '%s', class)"
                    !current str))
            end;
          | XName qu ->
            raise (Ast.TodoNamespace (Ast.info_of_qualified_ident qu))

         (* todo: do something here *)
          | Self _
          | Parent _
          | LateStatic _ ->
            ()
          )
        | _ -> ()
        );
        k x

      | Yield _ | YieldBreak _ ->
          add (P.Misc (spf "yield(%s)" !current));
          k x

      | _ -> k x
    );
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp (xhp_tag, _attrs, _tok, _, _)
      | XhpSingleton (xhp_tag, _attrs, _tok)
        ->
          let str = Cst_php.str_of_ident (Cst_php.XhpName xhp_tag) in
          (* use a different namespace than func? *)
          if not (Hashtbl.mem h str)
          then begin
            Hashtbl.replace h str true;
            add (P.Misc (spf "docall(%s, '%s', class)"  !current str))
          end;
          k x
    );
    V.kstmt = (fun (k, _) x ->
      (match x with
      | Throw (_, New (_, (Id (name)), _), _) ->
          add (P.Misc (spf "throw(%s, '%s')"
                 !current (Ast.str_of_name name)))
      | Try (_, _, cs, _) ->
          (cs) |> List.iter (fun (_, (_, (classname, _dname), _), _) ->
            add (P.Misc (spf "catch(%s, '%s')"
                   !current (Ast.str_of_class_name classname)))
          );
      | _ -> ()
      );
      k x
    );

  }
  in
  visitor (Program ast);
  ()


(*****************************************************************************)
(* Build db *)
(*****************************************************************************)

let build2 ?(show_progress=true) root files =
  ignore(show_progress);

  let res = ref [] in
  let add x = Common.push x res in

   add (P.Misc "% -*- prolog -*-");
   add (P.Misc ":- discontiguous kind/2, at/3, type/2");
   add (P.Misc ":- discontiguous file/2");
   add (P.Misc ":- discontiguous extends/2, implements/2, mixins/2");
   add (P.Misc ":- discontiguous docall/3, use/4, docall2/3");
   add (P.Misc ":- discontiguous is_public/1, is_private/1, is_protected/1");
   add (P.Misc ":- discontiguous static/1, abstract/1, final/1");
   add (P.Misc ":- discontiguous arity/2, parameter/4, return/2");
   add (P.Misc ":- discontiguous include/2, require_module/2");
   add (P.Misc ":- discontiguous throw/2, catch/2");
   add (P.Misc ":- discontiguous async/1, yield/1");
   add (P.Misc ":- discontiguous problem/2");

   add (P.Misc ":- discontiguous hh/2");
   (* see the comment on newv in add_uses() above *)
   add (P.Misc ":- discontiguous special/1");
   add (P.Misc "special('newv')");
   add (P.Misc "special('DT')");

   files |> List.iter (fun file ->

     let readable = Common.readable root file in
     let parts = Common.split "/" readable in
     add (P.Misc (spf "file('%s', [%s])" readable
           (parts |> List.map (fun s -> spf "'%s'" s) |> Common.join ",")));

     try
       let (ast, toks), _stat = Parse_php.parse file in

       (match toks with
       (* <?hh //xxx *)
       | T.T_OPEN_TAG ii1::T.TSpaces _::T.T_COMMENT ii2::_rest
         when PI.str_of_info ii1 =$= "<?hh" ->
           let s = PI.str_of_info ii2 in
           let mode =
             if s =~ "//[ \t]*\\([a-z]+\\)"
             then
               match Common.matched1 s with
               | ("strict" | "decl") as s -> s
               | "partial" -> "default"
               | _ -> "default"
             else "default"
           in
           add (P.Misc (spf "hh('%s', '%s')" readable mode))

       (* <?hh *)
       | Parser_php.T_OPEN_TAG info::_rest
         when PI.str_of_info info =$= "<?hh" ->
           add (P.Misc (spf "hh('%s', default)" readable));

       | _ -> ()
       );

       let ast = Unsugar_php.unsugar_self_parent_program ast in
       visit ~add readable ast;
     with Parsing.Parse_error | Cst_php.TodoNamespace _ ->
       add (P.Misc (spf "problem('%s', parse_error)" file))
   );


(*
   ));
*)

(*
   db.Db.uses.Db.includees_of_file#tolist +> List.iter (fun (file1, xs) ->
     let file1 = Db.absolute_to_readable_filename file1 db in
     xs +> List.iter (fun file2 ->
       let file2 =
         try Db.absolute_to_readable_filename file2 db
         with Failure _ -> file2
       in
       add (P.Misc (spf "include('%s', '%s')." file1 file2)
     );
   );
*)
   List.rev !res

let build ?show_progress a b =
  Common.profile_code "Prolog_php.build" (fun () -> build2 ?show_progress a b)

(* todo:
 * - could also improve precision of use/4
 * - detect higher order functions so that function calls through
 *   generic higher order functions are present in the callgraph
 *)
let append_callgraph_to_prolog_db2 ?(show_progress=true) g file =
  ignore(show_progress);

  (* look previous information, to avoid introduce duplication
   *
   * todo: check/compare with the basic callgraph I do in add_uses?
   * it should be a superset.
   *  - should find more functions when can resolve statically dynamic funcall
   *  -
   *)
  let h_oldcallgraph = Hashtbl.create 101 in
  file |> Common.cat |> List.iter (fun s ->
    if s =~ "^docall(.*"
    then Hashtbl.add h_oldcallgraph s true
  );

  Common2.with_open_outfile_append file (fun (pr, _chan) ->
    let pr s = pr (s ^ "\n") in
    pr "";
    g |> Map_.iter (fun src xs ->
      xs |> Set_.iter (fun target ->
        let kind =
          match target with
          (* can't call a file ... *)
          | CG.File _ -> raise Impossible
          (* can't call a fake root*)
          | CG.FakeRoot -> raise Impossible
          | CG.Function _ -> "function"
          | CG.Method _ -> "method"
        in
        (* do not count those fake edges *)
        (match src, target with
        | CG.FakeRoot, _
        | CG.File _, _ -> ()
        | _, CG.Function s when s =~ "__builtin" -> ()
        | _ ->
            let s1 =(spf "docall(%s, %s, %s)."
                       (name_of_node src) (name_of_node target) kind) in
            let s =(spf "docall2(%s, %s, %s)."
                       (name_of_node src) (name_of_node target) kind) in
            if Hashtbl.mem h_oldcallgraph s1
            then ()
            else pr s
        )
      )
    )
  )
let append_callgraph_to_prolog_db ?show_progress a b =
  Common.profile_code "Prolog_php.callgraph" (fun () ->
    append_callgraph_to_prolog_db2 ?show_progress a b)


(*****************************************************************************)
(* Query helpers *)
(*****************************************************************************)

(* copy paste of Common.cmd_to_list *)
let cmd_to_list_exit_1_ok command =
  let (l,exit_status) = Common2.process_output_to_list2 command in
  match exit_status with
  (* swipl uses 1 for error code when use fail *)
  | Unix.WEXITED (0 | 1) -> l
  | _ -> raise (CmdError (exit_status,
                         (spf "CMD = %s, RESULT = %s"
                             command (String.concat "\n" l))))

(* used for testing *)
let prolog_query ?(verbose=false) ~source_file ~query =
  let facts_pl_file = Common.new_temp_file "prolog_php_db" ".pl" in
  let helpers_pl_file =
    Config_pfff.path ^ "/h_program-lang/prolog_code.pl" in

  let show_progress = false in

  (* make sure it's a valid PHP file *)
  let _ast = Parse_php.parse_program source_file in

(*
  let g =
    Graph_code_php.build ~verbose (Right [source_file]) []
  in
*)

  let facts = build ~show_progress "/" [source_file] in
  Common.with_open_outfile facts_pl_file (fun (pr_no_nl, _chan) ->
    let pr s = pr_no_nl (s ^ "\n") in
    facts |> List.iter (fun fact ->
      pr (P.string_of_fact fact);
    )
  );

  let jujudb =
    Database_juju_php.juju_db_of_files ~show_progress [source_file] in
  let codedb =
    Database_juju_php.code_database_of_juju_db jujudb in
  let cg =
    Callgraph_php_build.create_graph ~show_progress [source_file] codedb in

  append_callgraph_to_prolog_db
    ~show_progress cg facts_pl_file;
  if verbose then Common.cat facts_pl_file |> List.iter pr2;
  let cmd =
    spf "swipl -s %s -f %s -t halt --quiet -g \"%s ,fail\""
      facts_pl_file helpers_pl_file query
  in
  let xs = cmd_to_list_exit_1_ok cmd in
  xs
