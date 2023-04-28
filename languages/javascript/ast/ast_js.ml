(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree for Javascript and (partially) Typescript.
 * (for a Concrete Syntax Tree see old/cst_js_ml or
 *  ocaml-tree-sitter-lang:javascript/lib/CST.ml).
 *
 * This file contains a simplified Javascript AST. The original
 * Javascript syntax tree (cst_js.ml) was good for code refactoring or
 * code visualization; the types used matches exactly the source. However,
 * for other algorithms, the nature of the CST made the code a bit
 * redundant. Hence the idea of a real and simplified AST
 * where certain constructions have been factorized or even removed.
 *
 * Here is a list of the simplications/factorizations:
 *  - no purely syntactical tokens in the AST like parenthesis, brackets,
 *    braces, angles, commas, semicolons, etc. No ParenExpr.
 *    The only token information kept is for identifiers for error reporting.
 *    See 'wrap' below.
 *    update: we actually keep the different kinds of brackets for sgrep, but
 *    they are all agglomerated in a general 'bracket' type.
 *  - no U, B, Yield, Await, Seq, ... just Apply (and Special Id)
 *  - no field vs method. A method is just sugar to define
 *    a field with a lambda (some people even uses directly that forms
 *    thx to arrows).
 *  - old: no Period vs Bracket (actually good to differentiate)
 *  - old: no Object vs Array (actually good to differentiate)
 *  - old: no type (actually need that back for semgrep-typescript)
 *  - no func vs method vs arrow, just a single function_definition type
 *  - no class elements vs object elements
 *  - No Nop (EmptyStmt); transformed in an empty Block,
 *  - optional pattern transpilation in transpile_js.ml
 *    (see Ast_js_build.transpile_pattern)
 *  - optional JSX transpilation
 *    (see Ast_js_build.transpile_xml)
 *  - no ForOf (see transpile_js.ml)
 *  - no ExportDefaultDecl, ExportDefaultExpr, just unsugared in
 *    separate variable declarations and an Export name
 *    (using 'default_entity' special name)
 *
 * todo:
 *  - typescript module
 *  - typescript enum
 *  - typescript declare
 *  - ...
 * less:
 *  - ast_js_es5.ml? unsugar even more? remove classes, get/set, etc.?
 *  - unsugar ES6 features? lift Var up, rename lexical vars, etc.
 *
 * Type aliases start with 'a_'. This is a hack to force the compiler to
 * report the original type name rather than an alias in error messages
 * and type hints.
 * See "-short-paths incorrectly picks arbitrary type alias" for details:
 * https://github.com/ocaml/ocaml/issues/10432
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type tok = Tok.t [@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok [@@deriving show]

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok [@@deriving show]
type a_todo_category = string wrap [@@deriving show]

(* real or fake when ASI (automatic semicolon insertion) *)
type sc = Tok.t [@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type a_ident = string wrap [@@deriving show]

type special =
  (* Special values *)
  | Null
  | Undefined (* builtins not in the grammar *)
  (* Special vars *)
  | This
  | Super
  (* CommonJS part1 *)
  | Exports
  | Module
  (* Asynchronous Module Definition (AMD) *)
  | Define
  (* Reflection *)
  | Arguments
  (* Special apply *)
  | NewTarget
  | Eval (* builtin not in the grammar *)
  | Seq
  (* a kind of cast operator:
   * See https://stackoverflow.com/questions/7452341/what-does-void-0-mean
   *)
  | Void
  | Typeof
  | Instanceof
  | In
  | Delete
  | Spread
  | Yield
  | YieldStar
  | Await
  (* This is used for "template literals". The boolean below represents
   * whether the template literal is part of a special "tagged" call.
   * If true, first arg of apply is the "tag".
   * Note that the example below is a call with a single string argument:
   *     foo(`a${b}c`) // we will generate an Encaps false here
   * The code below is a tagged template literal, which is a call with
   * N+1 arguments, where N is the number of interpolated values
   *     foo`a${b}c` // we will generate an Encaps true here.
   * Technically it's syntactic sugar for the following
   *     foo(['a', 'c'], b)
   * TODO: we might want to unsugar it at some point?
   *)
  | Encaps of bool
  (* CommonJS part2 *)
  | Require
  | UseStrict
  | ArithOp of AST_generic.operator
  (* less: should be in statement and unsugared in x+=1 or even x = x + 1 *)
  | IncrDecr of (AST_generic.incr_decr * AST_generic.prefix_postfix)
[@@deriving show { with_path = false }]

type a_label = string wrap [@@deriving show]

(* the filename is not "resolved".
 * alt: use a reference like for resolved_name set in graph_code_js.ml and
 * module_path_js.ml? *)
type a_filename = string wrap [@@deriving show]

(* Used for decorators and for TyName in AST_generic.type_.
 * Otherwise for regular JS dotted names are encoded with ObjAccess instead.
 *)
type a_dotted_ident = a_ident list [@@deriving show]

(* when doing export default Foo and import Bar, ... *)
let default_entity = "!default!"

type property_name =
  (* this can even be a string or number *)
  | PN of a_ident
  (* especially useful for array objects, but also used for dynamic fields *)
  | PN_Computed of expr
(* less: Prototype *)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
and expr =
  | L of literal
  | Id of a_ident
  | IdSpecial of special wrap
  (* old: we used to have a Nop, without any token attached, which allowed
   * to simplify a bit the AST by replacing some 'expr option' into simply
   * 'expr' (for v_init, ForClassic, Return) but this bited us in the long term
   * in semgrep where we don't want metavariables to match code that does
   * not exist.
   *)
  (* should be a statement ... *)
  | Assign of a_pattern * tok * expr
  (* less: could be transformed in a series of Assign(ObjAccess, ...) *)
  | Obj of a_obj
  (* we could transform it in an Obj but it can be useful to remember
   * the difference in further analysis (e.g., in the abstract interpreter).
   * This can also contain "holes" when the array is used in lhs of an assign
   * called "elision" which currently are skipped
   * TODO: have an (expr, elision) Common.either list bracket here.
   *)
  | Arr of expr list bracket
  (* ident is None when assigned in module.exports  *)
  | Class of class_definition * a_ident option
  | ObjAccess of expr * dot_operator wrap * property_name
  (* this can also be used to access object fields dynamically *)
  | ArrAccess of expr * expr bracket
  (* ident is a Some when recursive lambda or assigned in module.exports *)
  | Fun of function_definition * a_ident option
  | Apply of expr * a_arguments
  | New of tok * expr * a_arguments
  (* copy-paste of AST_generic.xml (but with different 'expr') *)
  | Xml of xml
  (* could unify with Apply, but need Lazy special then *)
  | Conditional of expr * expr * expr
  (* typescript: *)
  (* I'm not sure E : T is valid TS code actually; tree-sitter-ts allows it
   * but I can't find doc about it. Looks like the 'as' or <T> syntax
   * are the only "cast" syntax.
   *)
  | Cast of expr * tok (* ':' *) * type_
  | TypeAssert of expr * tok (* 'as' or '<' *) * type_ (* X as T or <T> X *)
  (* this is used mostly for unsupported typescript features *)
  | ExprTodo of a_todo_category * expr list
  (* sgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket
  | ObjAccessEllipsis of expr * tok (* ... *)
  | TypedMetavar of a_ident * tok * type_

(* TODO: add Null and Undefined here (move out of IdSpecial) *)
and literal =
  | Bool of bool wrap
  | Num of float option wrap
  | String of string wrap (* TODO: bracket, like for Regexp below *)
  | Regexp of string wrap bracket (* // *) * string wrap option (* modifier *)

and a_arguments = a_argument list bracket
and a_argument = expr (* see note about aliases prefixed by 'a_' *)

(* transpiled: to regular Calls when Ast_js_build.transpile_xml *)
and xml = {
  xml_kind : xml_kind;
  xml_attrs : xml_attribute list;
  xml_body : xml_body list;
}

and xml_kind =
  | XmlClassic of tok (*'<'*) * a_ident * tok (*'>'*) * tok (*'</foo>'*)
  | XmlSingleton of tok (*'<'*) * a_ident * tok (* '/>', with xml_body = [] *)
  | XmlFragment of tok (* '<>' *) * tok (* '</>', with xml_attrs = [] *)

and xml_attribute =
  | XmlAttr of a_ident * tok (* = *) * a_xml_attr_value
  (* jsx: usually a Spread operation, e.g., <foo {...bar} /> *)
  | XmlAttrExpr of expr bracket
  (* sgrep-ext: *)
  | XmlEllipsis of tok
(* either a String or a bracketed expr, but right now we just use expr *)

and a_xml_attr_value = expr (* see note about aliases prefixed by 'a_' *)

and xml_body =
  (* sgrep-ext: can contain "..." *)
  | XmlText of string wrap
  (* this can be None when people abuse {} to put comments in it *)
  | XmlExpr of expr option bracket
  | XmlXml of xml

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt =
  (* covers VarDecl, method definitions, class defs, etc *)
  | DefStmt of definition
  | Block of stmt list bracket
  | ExprStmt of expr * sc
    (* old: EmptyStmt of tok now transformed as an Block [] *)
  | If of tok * expr * stmt * stmt option
  | Do of tok * stmt * expr
  | While of tok * expr * stmt
  | For of tok * for_header * stmt
  | Switch of tok * expr * case list
  | Continue of tok * a_label option * sc
  | Break of tok * a_label option * sc
  | Return of tok * expr option * sc
  | Label of a_label * stmt
  | Throw of tok * expr * sc
  | Try of tok * stmt * catch option * (tok * stmt) option
  (* javascript special features, not in other imperative languages *)
  | With of tok * expr * stmt
  (* ES6 modules can appear only at the toplevel,
   * but CommonJS require() can be inside ifs
   * and tree-sitter-javascript accepts directives there too, so we allow
   * them at the stmt level too.
   * update: now toplevel = stmt, so definitely stmt-level material.
   *)
  | M of module_directive
  (* again, mostly used for unsupported typescript features *)
  | StmtTodo of a_todo_category * any list

(* less: could use some Special instead? *)
and for_header =
  | ForClassic of vars_or_expr * expr option * expr option
  (* TODO: tok option (* await *) *)
  | ForIn of var_or_expr * tok (* in *) * expr
  (* transpiled: when Ast_js_build.transpile_forof *)
  | ForOf of var_or_expr * tok (* of *) * expr
  (* sgrep-ext: *)
  | ForEllipsis of tok

(* the expr is usually just an assign *)
and vars_or_expr = (var list, expr) Common.either
and var_or_expr = (var, expr) Common.either
and case = Case of tok * expr * stmt | Default of tok * stmt

and catch =
  | BoundCatch of tok * a_pattern * stmt
  (* js-ext: es2019, catch {...} *)
  | UnboundCatch of tok * stmt

and dot_operator = Dot | QuestDot

(*****************************************************************************)
(* Pattern (destructuring binding) *)
(*****************************************************************************)
(* 'pattern' used to be a different type than 'expr' in cst_js.ml with
 * restrictions on what can be a pattern. But to simplify we now
 * reuse Obj, Arr, etc and so 'expr' to represent a pattern,
 * transpiled: to regular assignments when Ast_js_build.transpile_pattern.
 * sgrep: this is useful for sgrep to keep the ability to match over
 * JS destructuring patterns.
 *)
and a_pattern = expr (* see note about aliases prefixed by 'a_' *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* typescript-ext: old: was reusing AST_generic.type_ but can't anymore *)
and type_ =
  | TyBuiltin of string wrap
  (* todo: generics, * type_parameters list *)
  | TyName of a_dotted_ident
  (* fancy *)
  | TyLiteral of literal
  | TyQuestion of tok * type_
  | TyArray of type_ * (tok * unit * tok)
  | TyTuple of (tok * tuple_type_member list * tok)
  | TyFun of parameter list * type_ option
  | TyRecordAnon of (tok * property list * tok)
  | TyOr of type_ * tok * type_
  | TyAnd of type_ * tok * type_
  (* ex: KeyOf, This, *, LitType, LookupType, CondType, IndexKey, Typeof
   * MappedType, IsType, Infer, TemplateLitType *)
  | TypeTodo of a_todo_category * any list

and a_type_parameter = a_ident (* TODO: constraints *)
and a_type_parameter_constraint = type_

and tuple_type_member =
  (* simple tuple type element *)
  | TyTupMember of type_
  (* optional tuple type element in typescript: string? *)
  | TyTupOpt of type_ * tok
  (* rest tuple type element in typescript: ...string *)
  | TyTupRest of tok * type_

(*****************************************************************************)
(* Attributes *)
(*****************************************************************************)

(* quite similar to AST_generic.attribute but the 'argument' is different *)
and attribute =
  | KeywordAttr of keyword_attribute wrap
  (* a.k.a decorators *)
  | NamedAttr of tok (* @ *) * a_dotted_ident * a_arguments option

and keyword_attribute =
  (* field properties *)
  | Static
  (* todo? not in tree-sitter-js *)
  | Public
  | Private
  | Protected
  (* typescript-ext: for fields *)
  | Readonly
  | Optional
  (* '?' *)
  | NotNull (* '!' *)
  | Abstract (* also valid for class *)
  | Override (* since TS 4.3 *)
  (* method properties *)
  | Generator
  (* '*' *)
  | Async
  (* only inside classes *)
  | Get
  | Set

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

(* similar to what we do in AST_generic *)
and definition = entity * definition_kind

and entity = {
  (* ugly: can be AST_generic.special_multivardef_pattern when
   * Ast_js_build.transpile_pattern is false with a vinit an Assign itself.
   * actually in a ForIn/ForOf the init will be just the pattern, not even
   * an Assign.
   *)
  name : a_ident;
  (* TODO: put type parameters *)
  attrs : attribute list;
}

and definition_kind =
  | FuncDef of function_definition
  | VarDef of variable_definition
  | ClassDef of class_definition
  | DefTodo of a_todo_category * any list

and variable_definition = {
  v_kind : var_kind wrap;
  (* actually a pattern when inside a ForIn/ForOf *)
  v_init : expr option;
  (* typescript-ext: *)
  v_type : type_ option;
}

and var_kind = Var | Let | Const
and var = entity * variable_definition

and function_definition = {
  f_kind : AST_generic.function_kind wrap;
  (* less: move that in entity? but some anon func have attributes too *)
  f_attrs : attribute list;
  f_params : parameter list; (* TODO: bracket *)
  (* typescript-ext: *)
  f_rettype : type_ option;
  f_body : stmt;
}

and parameter =
  | ParamClassic of parameter_classic
  (* transpiled: when Ast_js_build.transpile_pattern
   * TODO: can also have types and default, so factorize with
   * parameter_classic?
   *)
  | ParamPattern of a_pattern
  (* sgrep-ext: *)
  | ParamEllipsis of tok

and parameter_classic = {
  p_name : a_ident;
  p_default : expr option;
  (* typescript-ext: *)
  p_type : type_ option;
  p_dots : tok option;
  p_attrs : attribute list;
}

(* expr is usually simply an Id
 * typescript-ext: can have complex type.
 * For an interface, the parent is always a type_
 * TODO: expr can have <type_arguments>
 *)
and parent = (expr, type_) Common.either

and class_definition = {
  (* typescript-ext: Interface is now possible *)
  c_kind : AST_generic.class_kind wrap;
  (* typescript-ext: can have multiple parents *)
  c_extends : parent list;
  (* typescript-ext: interfaces *)
  c_implements : type_ list;
  (* less: move in entity? *)
  c_attrs : attribute list;
  c_body : property list bracket;
}

and a_obj = property list bracket

and property =
  (* field_classic.fld_body is a (Some Fun) for methods.
   * None is possible only for class fields. For objects there is
   * always a value and it's using FieldColon instead of Field.
   *)
  | Field of field_classic
  | FieldColon of field_classic
  (* less: can unsugar? *)
  | FieldSpread of tok * expr
  (* This is present only when in pattern context.
   * ugly: we should have a clean separate pattern type instead of abusing
   * expr, which forces us to add this construct.
   *)
  | FieldPatDefault of a_pattern * tok * expr
  | FieldTodo of a_todo_category * stmt
  (* sgrep-ext: used for {fld1: 1, ... } which is distinct from spreading *)
  | FieldEllipsis of tok

and field_classic = {
  fld_name : property_name;
  fld_attrs : attribute list;
  fld_type : type_ option;
  fld_body : expr option;
}

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)
(* ES6 module directives appear only at the toplevel. However, for
 * CommomJS directives, some packages like react have dynamic imports
 * (to select dynamically which code to load depending on whether you run
 * in production or development environment) which means those directives
 * can be inside ifs.
 * update: for tree-sitter we allow them at the stmt level, hence the
 * recursive 'and' below.
 *)
and module_directive =
  (* 'ident' can be the special Ast_js.default_entity.
   * 'filename' is not "resolved"
   * (you may need for example to add node_modules/xxx/index.js
   * when you do 'import "react"' to get a resolved path).
   * See Module_path_js to resolve paths.
   *)
  | Import of tok * import_specifier list * a_filename
  | Export of tok * a_ident
  (* export * from 'foo'
     export * as bar from 'foo' *)
  | ReExportNamespace of tok * tok * a_ident option * tok * a_filename
  (* hard to unsugar in Import because we do not have the list of names *)
  | ModuleAlias of
      tok * a_ident * a_filename (* import * as 'name' from 'file' *)
  (* those should not exist (except for sgrep where they are useful),
   * unless file is a CSS file.
   *)
  | ImportFile of tok * a_filename

and import_specifier = a_ident * a_ident option

(* 'name1 as name2' *)
(*  [@@deriving show { with_path = false} ] *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
(* This used to be a special type with only var, stmt, or module_directive
 * but tree-sitter allows module directives at stmt level, and anyway
 * we don't enforce those constraints on the generic AST so simpler to
 * move those at the stmt level.
 *)
(* less: can remove and below when StmtTodo disappear *)
and a_toplevel = stmt
(* [@@deriving show { with_path = false} ] *)

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

and a_program = a_toplevel list
(* [@@deriving show { with_path = false} ] *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

and partial =
  (* partial defs. The stmt will be empty in f_body and c_body. *)
  | PartialDef of definition
  (* partial stmts *)
  | PartialIf of tok * expr
  | PartialTry of tok * stmt
  | PartialCatch of catch
  | PartialFinally of (tok * stmt)
  (* partial object, used only in JSON semgrep patterns for now *)
  | PartialSingleField of string wrap (* an id or str *) * tok (* : *) * expr
  (* not really a partial, but the partial machinery can help with that *)
  | PartialFunOrFuncDef of tok (* ... *) * function_definition
  | PartialSwitchCase of case

(* this is now mutually recursive with the previous types because of StmtTodo*)
and any =
  | Expr of expr
  | Stmt of stmt
  | Stmts of stmt list
  | Pattern of a_pattern
  | Property of property
  | Type of type_
  | Program of a_program
  | Partial of partial
  | Tk of tok
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* TODO: move in separate file? ast_js_parsing_helper.ml? *)

let mk_field name body =
  { fld_name = name; fld_body = body; fld_attrs = []; fld_type = None }

let mk_param id =
  { p_name = id; p_default = None; p_type = None; p_dots = None; p_attrs = [] }

let special_of_id_opt s =
  match s with
  | "eval" -> Some Eval
  | "undefined" -> Some Undefined
  (* commonJS *)
  | "require" -> Some Require
  | "exports" -> Some Exports
  | "module" -> Some Module
  (* AMD *)
  | "define" -> Some Define
  (* reflection *)
  | "arguments" -> Some Arguments
  | _ -> None

let idexp id = Id id

let idexp_or_special id =
  match special_of_id_opt (fst id) with
  | None -> idexp id
  | Some special -> IdSpecial (special, snd id)

(* note that this should be avoided as much as possible for sgrep, because
 * what was before a simple sequence of stmts in the same block can suddently
 * be in different blocks.
 *)
let stmt1_with b xs =
  match xs with
  | [] -> Block (b [])
  | [ x ] -> x
  | xs -> Block (b xs)

let stmt1 tok xs = stmt1_with (Tok.fake_bracket tok) xs
let unsafe_stmt1 xs = stmt1_with Tok.unsafe_fake_bracket xs
let basic_entity id = { name = id; attrs = [] }

let mk_default_entity_def tok exp =
  let n = (default_entity, tok) in
  (* TODO: look at exp and transform in FuncDef/ClassDef? *)
  let def =
    ( basic_entity n,
      VarDef { v_kind = (Const, tok); v_init = Some exp; v_type = None } )
  in
  (def, n)

let attr x = KeywordAttr x

(* helpers used in ast_js_build.ml and Parse_javascript_tree_sitter.ml *)
let var_pattern_to_var v_kind pat tok init_opt =
  match (pat, init_opt) with
  (* no need special_multivardef_pattern trick here *)
  | Id id, None -> (basic_entity id, { v_kind; v_init = None; v_type = None })
  | _ ->
      let s = AST_generic.special_multivardef_pattern in
      let id = (s, tok) in
      let init =
        match init_opt with
        | Some init -> Assign (pat, tok, init)
        | None -> pat
      in
      (* less: use x.vpat_type *)
      (basic_entity id, { v_kind; v_init = Some init; v_type = None })

let build_var kwd (id_or_pat, ty_opt, initopt) =
  match id_or_pat with
  | Left id ->
      (basic_entity id, { v_kind = kwd; v_init = initopt; v_type = ty_opt })
  | Right pat -> var_pattern_to_var kwd pat (snd kwd) initopt

let build_vars kwd vars = vars |> List.map (build_var kwd)
let vars_to_defs xs = xs |> List.map (fun (ent, v) -> (ent, VarDef v))
let vars_to_stmts xs = xs |> vars_to_defs |> List.map (fun x -> DefStmt x)

(*
   Left-handside patterns and function parameters happen to use the same
   syntax. This for converting one to the other.
*)
let parameter_to_pattern tok (param : parameter) : a_pattern =
  match param with
  | ParamClassic { p_name; p_default; p_type; p_dots; p_attrs } ->
      let pat =
        match p_dots with
        | None -> Id p_name
        | Some tok -> IdSpecial (Spread, tok)
      in
      let pat =
        match p_type with
        | None -> pat
        | Some type_ -> Cast (pat, Tok.fake_tok tok ":", type_)
      in
      let pat =
        match p_default with
        | None -> pat
        | Some expr -> Assign (pat, Tok.fake_tok tok "=", expr)
      in
      (* TODO? *)
      ignore p_attrs;
      pat
  | ParamPattern pat -> pat
  | ParamEllipsis tok -> Ellipsis tok

let mk_const_var id e =
  ( basic_entity id,
    VarDef { v_kind = (Const, snd id); v_init = Some e; v_type = None } )

let add_decorators_to_declaration decorators declaration =
  let ent, defkind = declaration in
  ({ ent with attrs = ent.attrs @ decorators }, defkind)

let add_decorators_to_declarations decorators declarations =
  List.map (add_decorators_to_declaration decorators) declarations

(*****************************************************************************)
(* Helpers. TODO: move in Tok.ml instead *)
(*****************************************************************************)

(* used both by Parsing_hacks_js and Parse_js *)
let fakeInfoAttach info =
  let info = Tok.rewrap_str "';' (from ASI)" info in
  let loc = Tok.unsafe_loc_of_tok info in
  Tok.FakeTokStr (";", Some (loc, -1))
