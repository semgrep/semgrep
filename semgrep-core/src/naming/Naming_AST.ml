(* Yoann Padioleau, Iago Abal
 *
 * Copyright (C) 2020-2022 r2c
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
open AST_generic
module V = Visitor_AST
module H = AST_generic_helpers

let logger = Logging.get_logger [ __MODULE__ ]

(* see error() below *)
let error_report = false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is to resolve names, a.k.a naming or
 * scope resolution, and to do it in a generic way on the generic AST.
 * update: this module is also (ab)used to propagate type information
 * used in semgrep for matching typed metavariables.
 *
 * In a compiler you often have those phases:
 *  - lexing
 *  - parsing
 *  - naming (the goal of this file)
 *  - typing
 *  - intermediate code generation
 *  - optimizing
 *  - ...
 *
 * The goal of the naming phase is to simplify following phases by having
 * each use of an entity clearly linked to its definition. For example,
 * when you see in the AST the use of the identifier 'a', this 'a'
 * could reference a local variable, or a parameter, or a global,
 * or a global defined in another module but imported in the current
 * namespace, or a variable defined in a nested block that "shadows" an
 * enclosing variable with the same name.
 * By resolving once and for all all uses of an entity to its definition,
 * for example by renaming some shadow variables (see AST_generic.gensym),
 * we simpify further phases, which don't have to maintain anymore a
 * complex environment to deal with scoping issues
 * (see the essence Of Python paper "Python: The Full Monty" where they
 * show that even complex IDEs still do not correctly handle Python
 * scoping rules and perform wrong renaming refactorings).
 *
 * Resolving names by tagging identifiers is also useful for
 * codemap/efuns to colorize identifiers (locals, params, globals, unknowns)
 * differently.
 *
 * alternatives:
 *  - CURRENT: generic naming and use of a 'ref resolved_name' to annotate
 *    the generic AST. Note that the use of a ref that can be shared with
 *    the lang-specific AST (e.g., ast_go.ml) allows tools like codemap/efuns
 *    to benefit from the generic naming analysis while still caring only
 *    about the lang-specific AST (even though we may want at some point
 *    to have a generic highlighter).
 *  - define a separate type for a named ast, e.g., nast.ml (as done in
 *    hack/skip) instead of modifying refs, with a unique identifier
 *    for each entity. However, this is tedious to
 *    write as both types are almost identical (maybe a functor could help,
 *    or a generic aast type as in recent hack code). Moreover, this is really
 *    useful for complex global analysis (or at least semi-global as in
 *    OCaml where you still need to open many .cmi when you locally type a .ml)
 *    such as typing where we want to resolve every use of a global.
 *    For semgrep, where we might for quite some time restrict ourselves to
 *    local analysis, maybe the ref implementation technique is good enough.
 *  - implement a resolve_xxx.ml for each language instead of doing it
 *    on the generic AST. That is what I was doing previously, which
 *    has some advantages (some language-specific constructs that introduce
 *    new variables, for example Python comprehensions, are hard to analyze
 *    once converted to the generic AST because they are under an
 *    Other_xxx category)
 *    update: actually comprehensions are now a regular AST element
 *    However, there's potentially lots of code
 *    duplication for each language and it's easy for a language to fall
 *    behind.
 *    A nice compromise might be to do most of the work in naming_ast.ml
 *    but still have lang-specific resolve_xxx.ml to tag special
 *    constructs that override what naming_ast.ml would do.
 *    See set_resolved()
 *
 * TODO:
 *  - generalize the original "resolvers":
 *    * resolve_go.ml
 *    * resolve_python.ml
 *    * ast_js_build.ml
 *    * check_variables_cpp.ml
 *    * check_variables_php.ml
 *  - introduce extra VarDef for languages that do not have them like
 *    Python/PHP where the first use is a def (which in turn requires
 *    special construct like 'global' or 'nonlocal' to disable this).
 *  - go:
 *    * handle DShortVars and Foreach local vars, DMethod receiver parameter,
 *      and TypeName for new types
 *    * in theory if/for/switch with their init declare new scope, as well
 *      as Block
 *    * should do first pass to get all toplevel decl as you can use
 *      forward ref in Go
 *  - get rid of the original "resolvers":
 *    * resolve_xxx.ml
 *    * ast_js_build.ml
 *    * check_variables_xxx.ml
 *  - get rid of or unify scope_code.ml, scope_php.ml, and
 *    ast_generic.resolved_name
 *  - resolve also types! in java if you import org.foo.Bar then later
 *    you can simply use Bar x; for a type, but we don't currently resolve
 *    those.
 *
 * history:
 *  - PHP deadcode detector with global analysis and global code database
 *  - local name resolution for PHP and C/C++ in check_variables_cpp.ml and
 *    check_variables_php.ml for codemap semantic highlighting of identifiers
 *    (mainly local vs params vs globals vs unknown) and for checkModule
 *    (scheck ancestor). Use of a ref for C/C++.
 *  - graph_code_xxx.ml global name resolution for PHP, then Java,
 *    then ML, then ML via cmt, then Clang ASTs, then C, then Javascript
 *  - separate named AST (nast.ml) and naming phase for Hack
 *  - local name resolution for code highlighting for Javascript, then Python
 *    to better colorize identifiers in codemap/efuns, but separate from
 *    a variable checker (resolve_xxx.ml instead of check_variables_xxx.ml)
 *  - AST generic and its resolved_name ref
 *  - simple resolve_python.ml with variable and import resolution
 *  - separate resolve_go.ml with import resolution
 *  - try to unify those resolvers in one file, naming_ast.ml
 *  - resolve names for OCaml constructs and factorize name resolution
 *    in better 'name' type and 'kname' hook.
 *)

(*****************************************************************************)
(* Scope *)
(*****************************************************************************)

(* this includes the "single unique id" (sid) *)
type resolved_name = AST_generic.resolved_name

type scope_info = {
  (* variable kind and sid *)
  entname : resolved_name;
  (* variable type, if known *)
  enttype : type_ option;
}

type scope = (string, scope_info) assoc

type scopes = {
  global : scope ref;
  (* function, nested blocks, nested functions (lambdas) *)
  blocks : scope list ref;
  (* useful for python, kind of global scope but for entities *)
  imported : scope ref;
      (* todo?
       * - class? right now we abuse EnclosedVar in resolved_name_kind.
       * - function? for 'var' in JS
       *)
}

let default_scopes () = { global = ref []; blocks = ref []; imported = ref [] }

(* because we use a Visitor instead of a clean recursive
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
 *)

let with_new_function_scope params scopes f =
  Common.save_excursion scopes.blocks (params :: !(scopes.blocks)) f

let with_new_block_scope scopes f =
  Common.save_excursion scopes.blocks ([] :: !(scopes.blocks)) f

let add_ident_current_scope (s, _) resolved scopes =
  match !(scopes.blocks) with
  | [] -> scopes.global := (s, resolved) :: !(scopes.global)
  | xs :: xxs -> scopes.blocks := ((s, resolved) :: xs) :: xxs

(* for Python *)
let add_ident_imported_scope (s, _) resolved scopes =
  scopes.imported := (s, resolved) :: !(scopes.imported)

let add_ident_global_scope (s, _) resolved scopes =
  scopes.global := (s, resolved) :: !(scopes.global)

(* for JS 'var' *)
let _add_ident_function_scope _id _resolved _scopes = raise Todo
let untyped_ent name = { entname = name; enttype = None }

(* see also lookup_scope_opt below taking as a parameter the environment *)
let rec lookup s xxs =
  match xxs with
  | [] -> None
  | xs :: xxs -> (
      match List.assoc_opt s xs with
      | None -> lookup s xxs
      | Some res -> Some res)

(* for Python, PHP *)
let lookup_global_scope (s, _) scopes = lookup s [ !(scopes.global) ]

(* for Python, PHP *)
let lookup_nonlocal_scope id scopes =
  let s, tok = id in
  match !(scopes.blocks) with
  | _ :: xxs -> lookup s xxs
  | [] ->
      let _ = error tok "no outerscope" in
      None

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)
type context =
  | AtToplevel
  | InClass
  (* separate InMethod? InLambda? just look for InFunction::InClass::_ *)
  | InFunction

type env = {
  ctx : context list ref;
  (* handle locals/params/globals, block vas, enclosed vars (closures).
   * handle also basic typing information now for Java/Go.
   *)
  names : scopes;
  in_lvalue : bool ref;
  in_type : bool ref;
  lang : Lang.t;
}

let default_env lang =
  {
    ctx = ref [ AtToplevel ];
    names = default_scopes ();
    in_lvalue = ref false;
    in_type = ref false;
    lang;
  }

(*****************************************************************************)
(* Environment Helpers *)
(*****************************************************************************)

let with_new_context ctx env f =
  Common.save_excursion env.ctx (ctx :: !(env.ctx)) f

let top_context env =
  match !(env.ctx) with
  | [] -> raise Impossible
  | x :: _xs -> x

let set_resolved env id_info x =
  (* TODO? maybe do it only if we have something better than what the
   * lang-specific resolved found?
   *)
  id_info.id_resolved := Some x.entname;
  (* This is defensive programming against the possibility of introducing
   * cycles in the AST.
   * Indeed, when we are inside a type, especially in  (OtherType (OT_Expr)),
   * we don't want set_resolved to set the type on some Id because
   * this could lead to cycle in the AST because of id_type
   * that will reference a type, that could containi an OT_Expr, containing
   * an Id, that could contain the same id_type, and so on.
   * See tests/python/naming/shadow_name_type.py for a patological example
   * See also tests/rust/parsing/misc_recursion.rs for another example.
   *)
  if not !(env.in_type) then id_info.id_type := x.enttype

(* accessors *)
let lookup_scope_opt (s, _) env =
  let scopes = env.names in

  let actual_scopes =
    match !(scopes.blocks) with
    | [] -> [ !(scopes.global); !(scopes.imported) ]
    | xs :: xxs -> (
        match env.lang with
        | Lang.Python ->
            if
              !(env.in_lvalue)
              (* just look current scope! no access to nested scopes or global *)
            then [ xs; !(scopes.imported) ]
            else [ xs ] @ xxs @ [ !(scopes.global); !(scopes.imported) ]
        (* | Lang.Php ->
             (* just look current scope! no access to nested scopes or global *)
             [xs;                            !(scopes.imported)]
        *)
        | _ -> [ xs ] @ xxs @ [ !(scopes.global); !(scopes.imported) ])
  in
  lookup s actual_scopes

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let error tok s =
  if error_report then raise (Parse_info.Other_error (s, tok))
  else logger#trace "%s at %s" s (Parse_info.string_of_info tok)

(*****************************************************************************)
(* Typing Helpers *)
(*****************************************************************************)

(* should use TyBuiltin instead? *)
let make_type type_string tok =
  Some (TyN (Id ((type_string, tok), empty_id_info ())) |> AST_generic.t)

(* This is only one part of the code to handle typed metavariables. Here
 * the goal is to help is setting the id_info.id_type for a few
 * identifiers in VarDef or Assign. Then, Generic_vs_generic.m_compatible_type
 * can leverage the info.
 *)
let get_resolved_type lang (vinit, vtype) =
  match vtype with
  | Some _ -> vtype
  | None -> (
      (* Should never be reached by languages where the type is in the declaration *)
      (* e.g. Java, C *)
      let string_str =
        match lang with
        | Lang.Go -> "str"
        | Lang.Js
        | Lang.Ts ->
            "string"
        | _ -> "string"
      in
      (* Currently these vary between languages *)
      (* Alternative is to define a TyInt, TyBool, etc in the generic AST *)
      (* so this is more portable across languages *)
      match vinit with
      | Some { e = L (Bool (_, tok)); _ } -> make_type "bool" tok
      | Some { e = L (Int (_, tok)); _ } -> make_type "int" tok
      | Some { e = L (Float (_, tok)); _ } -> make_type "float" tok
      | Some { e = L (Char (_, tok)); _ } -> make_type "char" tok
      | Some { e = L (String (_, tok)); _ } -> make_type string_str tok
      | Some { e = L (Regexp ((_, (_, tok), _), _)); _ } ->
          make_type "regexp" tok
      | Some { e = L (Unit tok); _ } -> make_type "unit" tok
      | Some { e = L (Null tok); _ } -> make_type "null" tok
      | Some { e = L (Imag (_, tok)); _ } -> make_type "imag" tok
      (* alt: lookup id in env to get its type, which would be cleaner *)
      | Some { e = N (Id (_, { id_type; _ })); _ } -> !id_type
      | Some { e = New (_, tp, (_, _, _)); _ } -> Some tp
      | _ -> None)

(*****************************************************************************)
(* Other Helpers *)
(*****************************************************************************)
let is_resolvable_name_ctx env lang =
  match top_context env with
  | AtToplevel
  | InFunction ->
      true
  | InClass -> (
      match lang with
      (* true for Java so that we can type class fields *)
      | Lang.Java
      (* true for JS/TS so that we can resolve class methods *)
      | Lang.Js
      | Lang.Ts ->
          true
      | _ -> false)

let resolved_name_kind env lang =
  match top_context env with
  | AtToplevel -> Global
  | InFunction -> LocalVar
  | InClass -> (
      match lang with
      (* true for Java so that we can type class fields.
       * alt: use a different scope.class?
       *)
      | Lang.Java
      (* true for JS/TS to resolve class methods. *)
      | Lang.Js
      | Lang.Ts ->
          EnclosedVar
      | _ -> raise Impossible)

(* !also set the id_info of the parameter as a side effect! *)
let params_of_parameters env xs =
  xs
  |> Common.map_filter (function
       | Param { pname = Some id; pinfo = id_info; ptype = typ; _ } ->
           let sid = H.gensym () in
           let resolved = { entname = (Parameter, sid); enttype = typ } in
           set_resolved env id_info resolved;
           Some (H.str_of_ident id, resolved)
       | _ -> None)

let declare_var env lang id id_info ~explicit vinit vtype =
  let sid = H.gensym () in
  (* for the type, we use the (optional) type in vtype, or, if we can infer
   * the type of the expression vinit (literal or id), we use that as a type
   * useful when the type is not given, e.g. in Go: `var x = 2` *)
  let resolved_type = get_resolved_type lang (vinit, vtype) in
  let name_kind, add_ident_to_its_scope =
    (* In JS/TS an assignment to a variable that has not been
     * previously declared will implicitly create a property on
     * the *global* object. *)
    if Lang.is_js lang && not explicit then (Global, add_ident_global_scope)
    else (resolved_name_kind env lang, add_ident_current_scope)
  in
  let resolved = { entname = (name_kind, sid); enttype = resolved_type } in
  add_ident_to_its_scope id resolved env.names;
  set_resolved env id_info resolved

let assign_implicitly_declares lang =
  lang = Lang.Python || lang = Lang.Ruby || lang = Lang.Php || Lang.is_js lang

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve lang prog =
  logger#trace "Naming_AST.resolve program";
  let env = default_env lang in

  (* coupling: we do similar things in Constant_propagation.ml so if you
   * add a feature here, you might want to add a similar thing over there too.
   *)
  let hooks =
    (* would be better to use a classic recursive-with-environment visit *)
    {
      V.default_visitor with
      (* ---------- *)
      (* !the defs! *)
      (* ---------- *)
      V.kfunction_definition =
        (fun (k, _v) x ->
          (* todo: add the function as a Global. In fact we should do a first
           * pass for some languages to add all of them first, because
           * Go for example allow the use of forward function reference
           * (no need to declarare prototype and forward decls as in C).
           *)
          let new_params = params_of_parameters env x.fparams in
          with_new_context InFunction env (fun () ->
              with_new_function_scope new_params env.names (fun () ->
                  (* todo: actually we should first go inside x.fparams.ptype
                   * without the new_params (this would also prevent cycle if
                   * a parameter name is the same than type name used in ptype
                   * (see tests/python/naming/shadow_name_type.py) *)
                  k x)));
      V.kclass_definition =
        (fun (k, _v) x ->
          (* todo: we should first process all fields in the class before
           * processing the methods, as some languages may allow forward ref.
           *)
          with_new_context InClass env (fun () -> k x));
      V.kdef =
        (fun (k, _v) x ->
          match x with
          | { name = EN (Id (id, id_info)); _ }, VarDef { vinit; vtype }
          (* note that some languages such as Python do not have VarDef
           * construct
           * todo? should add those somewhere instead of in_lvalue detection? *)
            when is_resolvable_name_ctx env lang ->
              (* Need to visit expressions first so that the id_type of
               * an id gets populated, e.g.
               * if we do var a = 3, then var b = a, we want to propagate the
               * type of a.
               * alt: do the lookup type in resolved_type
               *)
              k x;
              declare_var env lang id id_info ~explicit:true vinit vtype
          | { name = EN (Id (id, id_info)); _ }, FuncDef _
            when is_resolvable_name_ctx env lang ->
              (match lang with
              (* We restrict function-name resolution to JS/TS.
               *
               * Note that this causes problems with Python rule/test:
               *
               *     semgrep-rules/python/flask/correctness/same-handler-name.yaml
               *
               * This rule tries to match two different functions using the same
               * meta-variable. This works when the function names are not resolved,
               * but breaks when each function gets a unique sid.
               *
               * Function-name resolution is useful for interprocedural analysis,
               * feature that was requested by JS/TS users, see:
               *
               *     https://github.com/returntocorp/semgrep/issues/2787.
               *)
              | Lang.Js
              | Lang.Ts ->
                  let sid = H.gensym () in
                  let resolved =
                    untyped_ent (resolved_name_kind env lang, sid)
                  in
                  (* Previously we tried using add_ident_current_scope, but this
                   * shadowed imported function names which are added to the
                   * "imported" scope (globals/block scope is looked up first)
                   * even when the import statement comes after...
                   * This broke the following test:
                   *
                   *     semgrep-rules/python/django/security/audit/raw-query.py
                   *
                   * For now we add function names also to the "imported" scope...
                   * but do we need a special scope for imported functions?
                   *)
                  add_ident_imported_scope id resolved env.names;
                  set_resolved env id_info resolved
              | ___else___ -> ());
              k x
          | { name = EN (Id (id, id_info)); _ }, UseOuterDecl tok ->
              let s = Parse_info.str_of_info tok in
              let flookup =
                match s with
                | "global" -> lookup_global_scope
                | "nonlocal" -> lookup_nonlocal_scope
                | _ ->
                    error tok (spf "unrecognized UseOuterDecl directive: %s" s);
                    lookup_global_scope
              in
              (match flookup id env.names with
              | Some resolved ->
                  set_resolved env id_info resolved;
                  add_ident_current_scope id resolved env.names
              | None ->
                  error tok
                    (spf "could not find '%s' for directive %s"
                       (H.str_of_ident id) s));
              k x
          (* module L = List, in OCaml *)
          | ( { name = EN (Id (id, id_info)); _ },
              ModuleDef { mbody = ModuleAlias xs } ) ->
              (* similar to the ImportAs case *)
              let sid = H.gensym () in
              let resolved =
                untyped_ent (ImportedModule (DottedName xs), sid)
              in
              set_resolved env id_info resolved;
              (* difference with ImportAs, we add in local scope in OCaml *)
              add_ident_current_scope id resolved env.names;
              k x
          (* general case, just recurse *)
          | _ -> k x);
      (* sgrep: the import aliases *)
      V.kdir =
        (fun (k, _v) x ->
          (match x.d with
          | ImportFrom (_, DottedName xs, id, Some (alias, id_info)) ->
              (* for python *)
              let sid = H.gensym () in
              let resolved = untyped_ent (ImportedEntity (xs @ [ id ]), sid) in
              set_resolved env id_info resolved;
              add_ident_imported_scope alias resolved env.names
          | ImportFrom (_, DottedName xs, id, None) ->
              (* for python *)
              let sid = H.gensym () in
              let resolved = untyped_ent (ImportedEntity (xs @ [ id ]), sid) in
              add_ident_imported_scope id resolved env.names
          | ImportFrom (_, FileName (s, tok), id, None)
            when Lang.is_js lang && fst id <> Ast_js.default_entity ->
              (* for JS we consider import { x } from 'a/b/foo' as foo.x.
               * Note that we guard this code with is_js lang, because Python
               * uses also Filename in 'from ...conf import x'.
               *)
              let sid = H.gensym () in
              let _, b, _ = Common2.dbe_of_filename_noext_ok s in
              let base = (b, tok) in
              let resolved = untyped_ent (ImportedEntity [ base; id ], sid) in
              add_ident_imported_scope id resolved env.names
          | ImportFrom (_, FileName (s, tok), id, Some (alias, id_info))
            when Lang.is_js lang && fst id <> Ast_js.default_entity ->
              (* for JS *)
              let sid = H.gensym () in
              let _, b, _ = Common2.dbe_of_filename_noext_ok s in
              let base = (b, tok) in
              let resolved = untyped_ent (ImportedEntity [ base; id ], sid) in
              set_resolved env id_info resolved;
              add_ident_imported_scope alias resolved env.names
          | ImportAs (_, DottedName xs, Some (alias, id_info)) ->
              (* for python *)
              let sid = H.gensym () in
              let resolved =
                untyped_ent (ImportedModule (DottedName xs), sid)
              in
              set_resolved env id_info resolved;
              add_ident_imported_scope alias resolved env.names
          | ImportAs (_, FileName (s, tok), Some (alias, id_info)) ->
              (* for Go *)
              let sid = H.gensym () in
              let pkgname =
                let pkgpath, pkgbase = Common2.dirs_and_base_of_file s in
                if pkgbase =~ "v[0-9]+" then
                  (* e.g. google.golang.org/api/youtube/v3 *)
                  match pkgpath with
                  | [] -> pkgbase
                  | _ -> Common2.list_last pkgpath
                else if pkgbase =~ "\\(.+\\)-go" then
                  (* e.g. github.com/dgrijalva/jwt-go *)
                  matched1 pkgbase
                else (* default convention *)
                  pkgbase
              in
              let base = (pkgname, tok) in
              let resolved =
                untyped_ent (ImportedModule (DottedName [ base ]), sid)
              in
              set_resolved env id_info resolved;
              add_ident_imported_scope alias resolved env.names
          | _ -> ());
          k x);
      V.kcatch =
        (fun (k, _vout) x ->
          let _t, exn, _st = x in
          (match exn with
          (* TODO: we should create a new block scope *)
          | CatchParam { pname = Some id; pinfo = id_info; _ }
            when is_resolvable_name_ctx env lang ->
              declare_var env lang id id_info ~explicit:true None None
          | _ -> ());
          k x);
      V.kpattern =
        (fun (k, _vout) x ->
          match x with
          | PatId (id, id_info) when is_resolvable_name_ctx env lang ->
              (* todo: in Python it does not necessarily introduce
               * a newvar if the ID was already declared before.
               * Also inside a PatAs(PatId x,b), the 'x' is actually
               * the name of a class, not a newly introduced local.
               *)
              declare_var env lang id id_info ~explicit:true None None;
              k x
          | OtherPat _
          (* This interacts badly with implicit JS/TS declarations. It causes
           * `foo` in `function f({ foo }) { ... }` to be resolved as a global
           * variable, which in turn affects semgrep-rule _react-props-in-state_.
           * This when-clause achieves the previous behavior of leaving `foo`
           * unresolved. *)
          (* TODO: We should fix the AST of JS/TS so those `f({foo})` patterns do
           * not show as regular variables. *)
            when not (Lang.is_js lang) ->
              Common.save_excursion env.in_lvalue true (fun () -> k x)
          | _ -> k x);
      (* ---------- *)
      (* !the uses! *)
      (* ---------- *)
      (* kname will resolve names for Constructor/PatConstructor/NamedAttr/TyN
       * and maybe more. For expressions, we do something special for N (Id)
       * in kexpr do deal with languages where the first occurence of an
       * Id could be a declaration.
       *)
      V.kname =
        (fun (k, _) x ->
          match x with
          | Id (id, id_info) -> (
              match lookup_scope_opt id env with
              | Some resolved ->
                  (* name resolution *)
                  set_resolved env id_info resolved
              | _ -> ())
          | IdQualified
              {
                name_last = id, None;
                name_middle;
                name_info = id_info;
                name_top = None;
              } ->
              (match name_middle with
              (* this is quite specific to OCaml *)
              | Some (QDots [ (m, None) ]) -> (
                  match lookup_scope_opt m env with
                  | Some { entname = ImportedModule (DottedName xs), _sidm; _ }
                    ->
                      (* The entity is fully qualified, no need for sid *)
                      let sid = 0 in
                      let resolved =
                        untyped_ent (ImportedEntity (xs @ [ id ]), sid)
                      in
                      set_resolved env id_info resolved
                  | _ -> ())
              | _ -> ());
              k x
          | IdQualified _ -> ());
      V.kexpr =
        (fun (k, vout) x ->
          (* ugly: hack. If we use a classic recursive-with-env visitor,
           * we would not need this *)
          let recurse = ref true in
          (match x.e with
          (* Go: This is `x := E`, a single-variable short variable declaration.
           * When this declaration is legal, and that is when the same variable
           * has not yet been declared in the same scope, it *always* introduces
           * a new variable. (Quoting Go' spec, "redeclaration can only appear
           * in a multi-variable short declaration".)
           * See: https://golang.org/ref/spec#Short_variable_declarations *)
          | AssignOp ({ e = N (Id (id, id_info)); _ }, (Eq, tok), e2)
            when lang = Lang.Go
                 && Parse_info.str_of_info tok = ":="
                 && is_resolvable_name_ctx env lang ->
              (* Need to visit the RHS first so that type is populated *)
              (* If we do var a = 3, then var b = a, we want to propagate the type of a *)
              k x;
              declare_var env lang id id_info ~explicit:true (Some e2) None;
              recurse := false
          | Assign ({ e = N (Id (id, id_info)); _ }, _, e2)
            when Option.is_none (lookup_scope_opt id env)
                 && assign_implicitly_declares lang
                 && is_resolvable_name_ctx env lang ->
              (* Need to visit the RHS first so that type is populated *)
              vout (E e2);
              declare_var env lang id id_info ~explicit:false (Some e2) None;
              recurse := false
          (* todo: see lrvalue.ml
           * alternative? extra id_info tag?
           *)
          | Assign (e1, _, e2)
          | AssignOp (e1, _, e2) ->
              Common.save_excursion env.in_lvalue true (fun () -> vout (E e1));
              vout (E e2);
              recurse := false
          | ArrayAccess (e1, (_, e2, _)) ->
              vout (E e1);
              Common.save_excursion env.in_lvalue false (fun () -> vout (E e2));
              recurse := false
          (* specialized kname case when in expr context *)
          | N (Id (id, id_info)) ->
              (match lookup_scope_opt id env with
              | Some resolved ->
                  (* name resolution *)
                  set_resolved env id_info resolved
              | None ->
                  if
                    !(env.in_lvalue)
                    && assign_implicitly_declares lang
                    && is_resolvable_name_ctx env lang
                  then
                    (* first use of a variable can be a VarDef in some languages *)
                    declare_var env lang id id_info ~explicit:false None None
                  else
                    (* hopefully the lang-specific resolved may have resolved that *)
                    (* TODO: this can happen because of in_lvalue bug detection, or
                     * for certain entities like functions or classes which are
                     * currently tagged
                     *)
                    let s, tok = id in
                    error tok (spf "could not find '%s' in environment" s));
              recurse := false
          | DotAccess ({ e = IdSpecial (This, _); _ }, _, FN (Id (id, id_info)))
            -> (
              match lookup_scope_opt id env with
              (* TODO: this is a v0 for doing naming and typing of fields.
               * we should really use a different lookup_scope_class, that
               * would handle shadowing of fields from locals, etc. but it's
               * a start.
               *)
              | Some ({ entname = EnclosedVar, _sid; _ } as resolved) ->
                  set_resolved env id_info resolved
              | _ ->
                  let s, tok = id in
                  error tok (spf "could not find '%s' field in environment" s))
          | Comprehension (_op, (_l, (e, xs), _r)) ->
              (* Actually in Python2, no new scope was created, so iterator vars
               * could leak in the outer scope. This was fixed in Python3. *)
              with_new_block_scope env.names (fun () ->
                  (* first visit xs, then e *)
                  xs |> List.iter (fun x -> vout (ForOrIfComp x));
                  vout (E e));
              recurse := false
          | _ -> ());
          if !recurse then k x);
      V.ktype_ =
        (fun (k, _v) x ->
          if !(env.in_type) then k x
          else Common.save_excursion env.in_type true (fun () -> k x));
    }
  in
  let visitor = V.mk_visitor ~vardef_assign:false hooks in
  visitor (Pr prog);
  ()
  [@@profiling]
