(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
module E = Entity_code
module G = Graph_code
open Ast_java
module Ast = Ast_java
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Java. See graph_code.ml and main_codegraph.ml
 * for more information.
 *
 * choices:
 *  - package-based or dir-based schema? Seems simpler to use packages.
 *  - merge overloaded methods? yes, alternative is to mangle the
 *    name of the method with the type (a la C++ linker)
 *
 * schema:
 *   Package -> SubPackage -> Class (TODO | Interface )
 *                                    -> Method
 *                                    -> Field
 *                                    -> Constant (static final)
 *                                    -> Constant (enum, inlined in parent)
 *                            Class -> SubClass -> ...
 *                                          -> EnumSubClass (nothing)
 *   (when have no package)
 *   Dir -> Subdir -> File -> Class
 *
 *   PB -> Not_Found -> Package2 -> SubPackage2 -> ...
 *
 * note: adjust graph to remove intermediate singleton? com.xxx? Hmm better
 * to do that lazily in codegraph itself.
 *
 * note: doing codegraph for Java helps evaluate the number of lookup failures
 * in projects, and which code one needs to include to fully analyze the code.
 * If I go in the abstract interpreter path that julien took where he analyzed
 * code but had so many Not_found, Todo, exn, then I'll have no confidence
 * at all. So:
 *
 * - DONE lookup package correctly
 * - SEMI lookup classes correctly
 * - lookup field/methods correctly
 *
 * It also helps to find bug in the parser and better understand
 * Java and the AST :) e.g. Name -> Dot ambiguities.
 * It also helps to see which code is needed to fully analyze our code.
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  g : Graph_code.t;
  phase : phase;
  current : Graph_code.node;
  current_qualifier : Ast_java.qualified_ident;
  (* import x.y.* => [["x";"y"]; ...] *)
  imported_namespace : string list list;
  (* import x.y.z => [("z", (false, ["x";"y";"z"])); ...] *)
  imported_qualified : (string * (bool * Ast_java.qualified_ident)) list;
  (* This field is to avoid looking up parameters or locals in the graph.
   * We could also store them in the code graph so that the lookup
   * would work, but really fine-grained intra-method dependencies
   * are not that useful.
   *
   * The boolean final is because such locals/parameters should be
   * passed to anonymouse classes.
   *)
  params_or_locals : (string * bool (* is_final *)) list;
  (* To avoid looking up type parameters in the graph. *)
  type_parameters : string list;
}

(* We need 3 phases, one to get all the definitions, one to
 * get the inheritance information, and one to get all the Uses.
 * The inheritance is a kind of use, but certain uses like using
 * a field needs the full inheritance tree to already be computed
 * as we may need to lookup entities up in the parents.
 *)
and phase = Defs | Inheritance | Uses

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse ~show_parse_error file =
  try Parse_java.parse_program file with
  | Timeout _ as exn -> Exception.catch_and_reraise exn
  | exn ->
      let e = Exception.catch exn in
      if show_parse_error then
        pr2_once
          (spf "PARSE ERROR with %s, exn = %s" file (Exception.to_string e));
      []

let str_of_qualified_ident xs = xs |> List.map Ast.unwrap |> Common.join "."

let _str_of_name xs =
  xs
  |> List.map (fun (_tyarg_todo, ident) -> Ast.unwrap ident)
  |> Common.join "."

let unbracket (_, x, _) = x

(* helper to build entries in env.params_or_locals *)
let p_or_l v = (Ast.unwrap v.name, Ast.is_final v.mods)

(* TODO *)
let _long_ident_of_name xs = List.map snd xs

(* TODO *)
let long_ident_of_class_type xs = List.map fst xs

let nodeinfo ident =
  {
    G.pos = Parse_info.unsafe_token_location_of_info (Ast.info_of_ident ident);
    props = [];
    typ = None;
  }

let looks_like_class_name s = s =~ "[A-Z]"
let _looks_like_enum_constant s = s =~ "^[A-Z_0-9]+$"

let rec classname_and_info_of_typ t =
  match t with
  | TVar ii -> ("var", ii)
  | TBasic x -> x
  | TArray (_, t, _) -> classname_and_info_of_typ t
  | TClass xs ->
      let x = Common2.list_last xs in
      let ident, _args = x in
      ident

(* quite similar to create_intermediate_directories_if_not_present *)
let create_intermediate_packages_if_not_present g root xs =
  let dirs = Common2.inits xs |> List.map str_of_qualified_ident in
  let dirs =
    match dirs with
    | "" :: xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x :: xs ->
        let entity = (x, E.Package) in
        if G.has_node entity g then aux entity xs
        else (
          g |> G.add_node entity;
          g |> G.add_edge (current, entity) G.Has;
          aux entity xs)
  in
  aux root dirs

let add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (name, kind) in
  match () with
  | _ when not (G.has_node src env.g) ->
      pr2
        (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???"
           (G.string_of_node src) (G.string_of_node dst))
  | _ when G.has_node dst env.g -> G.add_edge (src, dst) G.Use env.g
  | _ -> (
      match kind with
      | _ -> (
          let kind_original = kind in
          let dst = (name, kind_original) in
          let parent_target = G.not_found in
          match kind_original with
          | E.Package ->
              let fake_package =
                Common.split "\\." name |> List.map (fun s -> s ^ "2")
              in
              let dst = (Common.join "." fake_package, kind_original) in
              if not (G.has_node dst env.g) then (
                create_intermediate_packages_if_not_present env.g parent_target
                  (fake_package |> List.map (fun s -> (s, ())));
                pr2
                  (spf "PB: lookup fail on %s (in %s)" (G.string_of_node dst)
                     (G.string_of_node src)));
              env.g |> G.add_edge (src, dst) G.Use;
              ()
          | _ ->
              pr2
                (spf "PB: lookup fail on %s (in %s)" (G.string_of_node dst)
                   (G.string_of_node src));
              G.add_node dst env.g;
              env.g |> G.add_edge (parent_target, dst) G.Has;
              env.g |> G.add_edge (src, dst) G.Use))

(*****************************************************************************)
(* Class/Package Lookup *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101

let lookup_fully_qualified_memoized env x =
  Common.profile_code "Graph_java.lookup_qualified" (fun () ->
      if env.phase = Uses || env.phase = Inheritance then
        Common.memoized _hmemo x (fun () ->
            Package_java.lookup_fully_qualified2 env.g x)
      else Package_java.lookup_fully_qualified2 env.g x)

(* Java allows to open namespaces by for instance importing packages
 * in which case we unsugar by preprending the package name.
 * Note that extending a class also imports its namespace (and
 * of all its parents too), hence import_of_inherited_classes below.
 *)
let with_full_qualifier env xs =
  env.imported_namespace
  |> List.map (fun qualified_ident ->
         let rev = List.rev qualified_ident in
         let prefix =
           (* todo: simplify now that have imported_qualified? *)
           match rev with
           | "*" :: rest -> List.rev rest
           (* todo opti: if head match the head of xs, then can accelerate things? *)
           | _ -> List.rev (List.tl rev)
         in
         prefix @ (xs |> List.map Ast.unwrap))

(* Look for entity (package/class/method/field) in list of imported
 * packages or in global scope. Return fully qualified entity.
 *
 * Note that the code graph store nodes in fully qualified form.
 *)
let (lookup2 : env -> Ast.qualified_ident -> Graph_code.node option) =
 fun env xs ->
  let candidates = with_full_qualifier env xs in
  (* pr2_gen candidates; *)
  candidates
  |> Common.find_some_opt (fun full_qualifier ->
         lookup_fully_qualified_memoized env full_qualifier)

let lookup a b = Common.profile_code "Graph_java.lookup" (fun () -> lookup2 a b)

(* pre: the Inheritance phase must have been done already
 * otherwise parents_inheritance can be empty or incomplete.
 *)
let rec import_of_inherited_classes env n =
  (* A class should Use only entities its extends or implements.
   * less: could filter out interface but needs them to store
   * then as E.Class E.Interface
   *)
  let parents_inheritance = G.succ n G.Use env.g in
  parents_inheritance
  |> Common.map_filter (fun (str, kind) ->
         match kind with
         | E.Class ->
             let xs = Common.split "\\." str @ [ "*" ] in
             let res = import_of_inherited_classes env (str, kind) in
             Some (xs :: res)
         | _ -> None)
  |> List.flatten

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
(* Note that there is no ~dupe argument. Java code uses packages and
 * fully qualified entities so there should be no name conflicts.
 *)
let rec extract_defs_uses ~phase ~g ~ast ~readable ~lookup_fails =
  ignore lookup_fails;

  let env =
    {
      g;
      phase;
      current =
        (match ast with
        | DirectiveStmt (Package (_, long_ident, _)) :: _ ->
            (str_of_qualified_ident long_ident, E.Package)
        | _ -> (readable, E.File));
      current_qualifier =
        (match ast with
        | DirectiveStmt (Package (_, long_ident, _)) :: _ -> long_ident
        | _ -> []);
      params_or_locals = [];
      type_parameters = [];
      imported_namespace =
        (match ast with
        (* we automatically import the current.package.* *)
        | DirectiveStmt (Package (_, long_ident, _)) :: _ ->
            [ List.map Ast.unwrap long_ident @ [ "*" ] ]
        | _ -> [])
        @ (ast
          |> Common.map_filter (function
               | DirectiveStmt (Import (_is_static, _import)) ->
                   (* List.map Ast.unwrap qualified_ident *) raise Todo
               | _ -> None))
        @ [
            (* we automatically import java.lang.* *)
            [ "java"; "lang"; "*" ];
            (* we automatically import top packages *)
            [ "*" ];
          ];
      imported_qualified =
        ast
        |> Common.map_filter (function
             | DirectiveStmt (Import (_is_static, _import)) -> raise Todo
             | _ ->
                 None
                 (*
      match List.rev xs with
      | [] -> raise Impossible
      | ["*", _] -> None
      | (s, _)::_rest -> Some (s, (is_static, xs))
*));
    }
  in

  (if phase = Defs then
   match ast with
   | DirectiveStmt (Package (_, long_ident, _)) :: _ ->
       create_intermediate_packages_if_not_present g G.root long_ident
   (* have None usually for scripts, tests, or entry points *)
   | _ ->
       let dir = Common2.dirname readable in
       G.create_intermediate_directories_if_not_present g dir;
       g |> G.add_node (readable, E.File);
       g |> G.add_edge ((dir, E.Dir), (readable, E.File)) G.Has);
  (* double check if we can find some of the imports
   * (especially useful when have a better java_stdlib/ to report
   * third-party packages not-yet handled).
   *)
  if phase = Inheritance then
    ast
    |> List.iter (function
         | DirectiveStmt (Import (_is_static, _import)) -> (
             let qualified_ident_bis =
               raise Todo
               (*
        match List.rev qualified_ident with
        | ("*",_)::rest -> List.rev rest
        (* less: just lookup the class for now *)
        | _x::xs when is_static -> List.rev xs
        | _ -> qualified_ident
      *)
             in
             let entity = List.map Ast.unwrap qualified_ident_bis in
             match lookup_fully_qualified_memoized env entity with
             | Some _ ->
                 (* no need add_use_edge here, it will be done later when
                  * one use the entity
                  * less: could be used to detect useless import
                  *)
                 ()
             | None ->
                 pr2_once
                   (spf "PB: wrong import: %s"
                      (str_of_qualified_ident qualified_ident_bis)))
         | _ -> ());

  (* imports is not the only way to use external packages, one can
   * also just qualify the classname or static method so we need
   * to visit the AST and lookup classnames (possibly using information
   * from the import to know where to look for first).
   *)
  stmts env ast

(* ---------------------------------------------------------------------- *)
(* Declarations (classes, fields, etc) *)
(* ---------------------------------------------------------------------- *)
and decl env = function
  | Class def, _ -> class_decl env def
  | Method def, _ -> method_decl env def
  | Field def, _ -> field_decl env def
  | Enum def, _ -> enum_decl env def
  | DeclEllipsis _, _ -> ()
  | Init (_is_static, st), n ->
      let name = spf "__init__%d" n in
      let full_ident = env.current_qualifier @ [ (name, fakeInfo name) ] in
      let full_str = str_of_qualified_ident full_ident in
      let node = (full_str, E.TopStmts) in
      if env.phase = Defs then (
        env.g |> G.add_node node;
        env.g |> G.add_edge (env.current, node) G.Has);
      let env = { env with current = node; current_qualifier = full_ident } in
      stmt env st
  | EmptyDecl _, _ -> ()
  | AnnotationTypeElementTodo _, _ -> raise Todo

and decls env xs = List.iter (decl env) (Common.index_list_1 xs)

and class_decl env def =
  let full_ident = env.current_qualifier @ [ def.cl_name ] in
  let full_str = str_of_qualified_ident full_ident in
  let node = (full_str, E.Class) in
  if env.phase = Defs then (
    (* less: def.c_type? *)
    env.g |> G.add_node node;
    env.g |> G.add_nodeinfo node (nodeinfo def.cl_name);
    env.g |> G.add_edge (env.current, node) G.Has);
  let env =
    {
      env with
      current = node;
      current_qualifier = full_ident;
      (* with anon classes we need to lookup enclosing final parameters/locals *)
      params_or_locals = env.params_or_locals |> List.filter (fun (_x, b) -> b);
      type_parameters =
        def.cl_tparams
        |> List.map (function
             | TParamEllipsis _ -> raise Impossible
             | TParam ((str, _tok), _constraints) -> str);
    }
  in
  let parents = Common2.option_to_list def.cl_extends @ def.cl_impls in
  List.iter (typ env) parents;

  let imports =
    if env.phase = Defs then []
    else
      (* Java allows programmer to use fields without qualifying them
       * (without a class.xxx, or this.xxx) so we need to unsugar this
       * by prepending the full current classname. We can just
       * generate a fake import package.classname.*. This will also
       * allow nested classes to access siblings.
       *)
      (List.map Ast.unwrap full_ident @ [ "*" ])
      :: import_of_inherited_classes env (full_str, E.Class)
  in
  decls
    { env with imported_namespace = imports @ env.imported_namespace }
    (unbracket def.cl_body)

(* Java allow some forms of overloading, so the same method name can be
 * used multiple times.
 *)
and method_decl env def =
  let full_ident = env.current_qualifier @ [ def.m_var.name ] in
  let full_str = str_of_qualified_ident full_ident in
  let node = (full_str, E.Method) in
  if env.phase = Defs then
    if
      (* less: static? *)
      (* less: for now we just collapse all methods with same name together *)
      G.has_node (full_str, E.Method) env.g
    then ()
    else (
      env.g |> G.add_node node;
      env.g |> G.add_nodeinfo node (nodeinfo def.m_var.name);
      env.g |> G.add_edge (env.current, node) G.Has);
  let env =
    {
      env with
      current = node;
      (* No change to the qualifier? methods are not a namespace?
       * Hmm but can have nested classes inside a methods that
       * share the same name so yes need full_ident as a qualifier.
       *)
      current_qualifier = full_ident;
      params_or_locals =
        (def.m_formals
        |> Common.map_filter (function
             | ParamClassic p
             | ParamReceiver p
             | ParamSpread (_, p) ->
                 Some p
             | ParamEllipsis _ -> None)
        |> List.map p_or_l)
        @ (* with methods of anon classes we need to lookup enclosing
           * final parameters/locals
           *)
        (env.params_or_locals |> List.filter (fun (_x, b) -> b));
      (* TODO use m_tparams *)
      type_parameters = [];
    }
  in
  var env def.m_var;
  def.m_formals
  |> List.iter (function
       | ParamEllipsis _ -> ()
       | ParamClassic v
       | ParamReceiver v
       | ParamSpread (_, v) ->
           var env v);
  (* todo: m_throws *)
  stmt env def.m_body

and field_decl env def =
  let full_ident = env.current_qualifier @ [ def.f_var.name ] in
  let full_str = str_of_qualified_ident full_ident in
  let kind =
    if Ast.is_final_static def.f_var.mods then E.Constant else E.Field
  in
  let node = (full_str, kind) in
  if env.phase = Defs then (
    (* less: static? *)
    env.g |> G.add_node node;
    env.g |> G.add_nodeinfo node (nodeinfo def.f_var.name);
    env.g |> G.add_edge (env.current, node) G.Has);
  let env =
    { env with current = node; current_qualifier = env.current_qualifier }
  in
  field env def

and enum_decl env def =
  let full_ident = env.current_qualifier @ [ def.en_name ] in
  let full_str = str_of_qualified_ident full_ident in
  (* less: make it a class? or a Type? *)
  let node = (full_str, E.Class) in
  if env.phase = Defs then (
    env.g |> G.add_node node;
    env.g |> G.add_nodeinfo node (nodeinfo def.en_name);
    env.g |> G.add_edge (env.current, node) G.Has);
  let env =
    {
      env with
      current = node;
      current_qualifier = full_ident;
      params_or_locals = [];
      (* TODO *)
      type_parameters = [];
    }
  in
  let parents = def.en_impls in
  List.iter (typ env) parents;
  let csts, xs = def.en_body in
  decls env xs;

  csts
  |> List.iter (fun (ident, args_opt, body_opt) ->
         let full_ident = env.current_qualifier @ [ ident ] in
         let full_str = str_of_qualified_ident full_ident in
         let node = (full_str, E.Constant) in
         if env.phase = Defs then (
           env.g |> G.add_node node;
           env.g |> G.add_nodeinfo node (nodeinfo ident);
           env.g |> G.add_edge (env.current, node) G.Has);
         let env =
           { env with current = node; current_qualifier = full_ident }
         in
         args_opt |> Option.iter (fun (_, xs, _) -> exprs env xs);
         body_opt |> Option.iter (fun (_, xs, _) -> decls env xs))

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
(* mostly boilerplate, control constructs don't introduce entities *)
and stmt env = function
  | EmptyStmt _ -> ()
  | Block (_, xs, _) -> stmts env xs
  | Expr (e, _) -> expr env e
  | If (_, e, st1, st2) ->
      expr env e;
      stmts env (st1 :: Option.to_list st2)
  | Switch (_, e, xs) ->
      expr env e;
      xs
      |> List.iter (fun (cs, sts) ->
             cases env cs;
             stmts env sts)
  | While (_, e, st) ->
      expr env e;
      stmt env st
  | Do (_, st, e) ->
      expr env e;
      stmt env st
  | For (_, x, st) ->
      let env =
        match x with
        | ForEllipsis _t -> env
        | Foreach (v, e) ->
            var env v;
            expr env e;
            { env with params_or_locals = p_or_l v :: env.params_or_locals }
        | ForClassic (init, es1, es2) -> (
            match init with
            | ForInitExprs es0 ->
                exprs env (es0 @ es1 @ es2);
                env
            | ForInitVars xs ->
                List.iter (field env) xs;
                let env =
                  {
                    env with
                    params_or_locals =
                      (xs |> List.map (fun fld -> p_or_l fld.f_var))
                      @ env.params_or_locals;
                  }
                in
                exprs env (es1 @ es2);
                env)
      in
      stmt env st
      (* could have an entity and dependency ... but it's intra procedural
       * so not that useful
       *)
  | Label (_id, st) -> stmt env st
  | Break (_, _idopt)
  | Continue (_, _idopt) ->
      ()
  | Return (_, eopt) -> exprs env (Common2.option_to_list eopt)
  | Sync (_, e, st) ->
      expr env e;
      stmt env st
  | Try (_, _resourcesTODO, st, xs, stopt) ->
      stmt env st;
      catches env xs;
      stopt |> Option.iter (fun (_, st) -> stmt env st)
  | Throw (_, e) -> expr env e
  | Assert (_, e, eopt) -> exprs env (e :: Common2.option_to_list eopt)
  (* The modification of env.params_locals is done in decls() *)
  | LocalVarList xs -> List.iter (field env) xs
  | DeclStmt x -> decl env (x, 0)
  | DirectiveStmt _ -> raise Todo

and stmts env xs =
  let rec aux env = function
    | [] -> ()
    | x :: xs ->
        stmt env x;
        let env =
          match x with
          | LocalVarList flds ->
              List.fold_right
                (fun fld env ->
                  {
                    env with
                    params_or_locals = p_or_l fld.f_var :: env.params_or_locals;
                  })
                flds env
          (* also add LocalClass case? no, 'lookup env ...' handles that *)
          | _ -> env
        in
        aux env xs
  in
  aux env xs

and cases env xs = List.iter (case env) xs

and case env = function
  | Case (_, e) -> expr env e
  | Default _ -> ()

and catches env xs = List.iter (catch env) xs

and catch env (_, catch_exn, st) =
  match catch_exn with
  | CatchParam (v, _uniontypes) ->
      var env v;
      let env =
        { env with params_or_locals = p_or_l v :: env.params_or_locals }
      in
      stmt env st
  | CatchEllipsis _ -> stmt env st

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
  (* main dependency source! *)
  | This _ -> ()
  | NameId _ -> failwith "TODO: look Name below"
  (*
  | Name n ->
      if env.phase = Uses then begin
        let str = str_of_name n in
        (match str, n with
         (* TODO: look at the type and continue lookup *)
         | _, (_,(s,_))::_rest when List.mem_assoc s env.params_or_locals -> ()
         (* TODO *)
         | "super", _ | "this", _ ->
             ()
         | _ ->
             (match lookup env (long_ident_of_name n) with
              | Some n2 ->
                  add_use_edge env n2
              | None ->
                  (match n with
                   | [] ->
                       pr2 "Name is empty??";
                       pr2_gen (env.current, n);
                       raise Impossible
                   | (_, (s,_))::_ when List.mem_assoc s env.imported_qualified ->
                       let (_is_static, full_ident) =
                         List.assoc s env.imported_qualified in
                       let str = str_of_qualified_ident full_ident in
                       add_use_edge env (str, E.Package)

                   | [_x] when looks_like_enum_constant str ->
                       pr2 ("PB: " ^ Common.dump n);
                   | [_x] when looks_like_class_name str ->
                       add_use_edge env (str, E.Package)
                   | [_x] ->
                       pr2 ("PB: " ^ Common.dump n);
                       (* env.imported_namespace +> List.iter pr2_gen; *)
                   | _x::_y::_xs ->
                       (* unknown package probably *)
                       add_use_edge env (str, E.Package)
                  )
             )
        )
      end
*)
  | NameOrClassType _ -> ()
  | Literal _ -> ()
  | ClassLiteral (t, _) -> typ env t
  | NewClass (tok, t, (_, args, _), decls_opt) -> (
      typ env t;
      exprs env args;
      match decls_opt with
      | None -> ()
      | Some xs ->
          (* less: quite similar to class_decl, factorize code? *)
          let classname, info = classname_and_info_of_typ t in
          let charpos = PI.pos_of_info info in
          let anon_class = spf "__anon__%s__%d" classname charpos in
          let cdecl =
            {
              cl_name = (anon_class, info);
              cl_extends = Some t;
              cl_impls = [];
              cl_kind = (ClassRegular, tok);
              cl_body = xs;
              (* ?? *)
              cl_tparams = [];
              cl_mods = [];
              cl_formals = [];
            }
          in
          class_decl env cdecl)
  | NewQualifiedClass (_e, _t1, tok, ty, args, decls_opt) ->
      (*
      pr2 "NewQualifiedClass";
      pr2_gen (NewQualifiedClass (e, id, args, decls_opt));
      *)
      (* todo: need to resolve the type of 'e' *)
      expr env (NewClass (tok, ty, args, decls_opt))
  | NewArray (_tok, t, args, _i, ini_opt) ->
      typ env t;
      exprs env args;
      init_opt env ini_opt
  | Call (e, (_, es, _)) ->
      expr env e;
      exprs env es
  | Dot (e, _t, _idTODO) ->
      (* todo: match e, and try lookup method/field
       * if e is a Name, lookup it, and if a class then
       * lookup children. If local ... then need get its type
       * lookup its node, and then lookup children.
       *)
      expr env e
  | ArrayAccess (e1, (_, e2, _)) -> exprs env [ e1; e2 ]
  | Postfix (e, _)
  | Prefix (_, e)
  | Unary (_, e) ->
      expr env e
  | Infix (e1, _op, e2) -> exprs env [ e1; e2 ]
  | Conditional (e1, e2, e3) -> exprs env [ e1; e2; e3 ]
  | SwitchE (_tok, e, _cases) -> expr env e
  | AssignOp (e1, _op, e2) -> exprs env [ e1; e2 ]
  | Assign (e1, _tok, e2) -> exprs env [ e1; e2 ]
  | TypedMetavar (_ident, _typ) -> ()
  | Cast ((_, t, _), e) ->
      List.iter (typ env) t;
      expr env e
  | InstanceOf (e, tref) ->
      expr env e;
      typ env tref
  | Ellipsis _
  | DeepEllipsis _
  | ObjAccessEllipsis _ ->
      ()
  | Lambda (_params, _t, _st) -> raise Todo (* imitate method_decl code *)
  | MethodRef _ -> raise Todo

and exprs env xs = List.iter (expr env) xs

and init env = function
  | ExprInit e -> expr env e
  | ArrayInit xs -> List.iter (init env) (unbracket xs)

and init_opt env opt =
  match opt with
  | None -> ()
  | Some ini -> init env ini

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)
and typ env = function
  | TBasic _
  | TVar _ ->
      ()
  | TArray (_, t, _) -> typ env t
  (* other big dependency source! *)
  | TClass reft -> (
      (* todo: let's forget generic arguments for now *)
      let xs = long_ident_of_class_type reft in
      let str = str_of_qualified_ident xs in
      if env.phase = Uses || env.phase = Inheritance then
        match (str, reft) with
        (* TODO: look at the type and continue lookup *)
        | _, ((s, _), _) :: _rest when List.mem s env.type_parameters -> ()
        | _ -> (
            match lookup env xs with
            (* TODO: look in type_params_local ! *)
            | Some n2 ->
                (* pr2 ("FOUND: " ^ Common.dump n); *)
                add_use_edge env n2
            | None -> (
                match xs with
                | [] -> raise Impossible
                | (s, _) :: _ when List.mem_assoc s env.imported_qualified ->
                    let _is_static, full_ident =
                      List.assoc s env.imported_qualified
                    in
                    let str = str_of_qualified_ident full_ident in
                    add_use_edge env (str, E.Package)
                | [ _x ] ->
                    if looks_like_class_name str then
                      add_use_edge env (str, E.Package)
                    else pr2 ("PB: " ^ Dumper.dump reft)
                | _x :: _y :: _xs ->
                    (* unknown package probably *)
                    add_use_edge env (str, E.Package))))

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)
and var env v =
  Option.iter (typ env) v.type_;
  ()

and field env f =
  var env f.f_var;
  init_opt env f.f_init;
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose = true) ?(only_defs = false) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  let lookup_fails = Common2.hash_with_default (fun () -> 0) in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files
  |> Console.progress ~show:verbose (fun k ->
         List.iter (fun file ->
             k ();
             let readable = Common.readable ~root file in
             let ast = parse ~show_parse_error:true file in
             extract_defs_uses ~phase:Defs ~g ~ast ~readable ~lookup_fails));
  if not only_defs then (
    (* step2: creating the 'Use' edges just for inheritance *)
    if verbose then pr2 "\nstep2: extract inheritance information";
    files
    |> Console.progress ~show:verbose (fun k ->
           List.iter (fun file ->
               k ();
               let readable = Common.readable ~root file in
               let ast = parse ~show_parse_error:false file in
               extract_defs_uses ~phase:Inheritance ~g ~ast ~readable
                 ~lookup_fails));

    (* step3: creating the 'Use' edges that can rely on recursive inheritance *)
    if verbose then pr2 "\nstep3: extract uses";
    files
    |> Console.progress ~show:verbose (fun k ->
           List.iter (fun file ->
               k ();
               let readable = Common.readable ~root file in
               let ast = parse ~show_parse_error:false file in
               extract_defs_uses ~phase:Uses ~g ~ast ~readable ~lookup_fails)));
  g
