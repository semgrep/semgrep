(* Yoann Padioleau
 *
 * Copyright (C) 2019, 2020 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A (real) Abstract Syntax Tree for Javascript, not a Concrete Syntax Tree
 * as in cst_js.ml.
 * 
 * This file contains a simplified Javascript AST. The original
 * Javascript syntax tree (cst_js.ml) is good for code refactoring or
 * code visualization; the types used matches exactly the source. However,
 * for other algorithms, the nature of the CST makes the code a bit
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
 *  - no types (but could revisit this decision)
 *  - no Typescript (no interface)
 *  - no U, B, Yield, Await, Seq, ... just Apply (and Special Id)
 *  - no field vs method. A method is just sugar to define
 *    a field with a lambda (some people even uses directly that forms
 *    thx to arrows).
 *  - old: no Period vs Bracket (actually good to differentiate)
 *  - old: no Object vs Array (actually good to differentiate)
 *  - no func vs method vs arrow, just fun_
 *  - no class elements vs object elements
 *  - No Nop (EmptyStmt); transformed in an empty Block,
 *  - no patterns (they are transpiled, see transpile_js.ml, unless
 *    Ast_js_build.transpile_pattern is false)
 *  - no JSX (see transpile_js.ml, unless Ast_js_build.transpile_xml is false)
 *  - no ForOf (see transpile_js.ml)
 *  - no ExportDefaultDecl, ExportDefaultExpr, just unsugared in
 *    separate variable declarations and an Export name
 *    (using 'default_entity' special name)
 * 
 * todo:
 *  - add back type information? useful for many analysis! 
 *    useful for semgrep for typescript!
 *  - add decorators (also useful for semgrep) 
 *  - ast_js_es5.ml? unsugar even more? remove classes, get/set, etc.?
 *  - unsugar ES6 features? lift Var up, rename lexical vars, etc.
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * transformation field that makes possible spatch on the code.
 *)
type tok = Parse_info.t
 [@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok
 [@@deriving show] (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
 [@@deriving show] (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type ident = string wrap
 [@@deriving show]

(* For bar() in a/b/foo.js the qualified_name is 'a/b/foo.bar'. 
 * I remove the filename extension for codegraph (which assumes
 * the dot is a package separator), which is convenient to show 
 * shorter names when exploring a codebase (and maybe also when hovering
 * a function in codemap).
 * This is computed after ast_js_build in graph_code_js.ml
 *)
type qualified_name = string
 [@@deriving show] (* with tarzan *)

(* todo: use AST_generic.resolved_name at some point, and share the ref! *)
type resolved_name =
  (* this can be computed by ast_js_build.ml *)
  | Local
  | Param
  (* this is computed in graph_code_js.ml in a "naming" phase *)
  | Global of qualified_name
  (* default case *)
  | NotResolved
 [@@deriving show { with_path = false} ] (* with tarzan *)

type special = 
  (* Special values *)
  | Null | Undefined (* builtin not in grammar *)

  (* Special vars *)
  | This | Super
  (* CommonJS part1 *)
  | Exports | Module
  (* Asynchronous Module Definition (AMD) *)
  | Define
  (* Reflection *)
  | Arguments

  (* Special apply *)
  | New | NewTarget
  | Eval (* builtin not in grammar *)
  | Seq 
  (* a kind of cast operator: 
   * See https://stackoverflow.com/questions/7452341/what-does-void-0-mean
   *)
  | Void
  | Typeof | Instanceof
  | In | Delete 
  | Spread
  | Yield | YieldStar | Await
  | Encaps of bool (* if true, first arg of apply is the "tag" *)
  (* CommonJS part2 *)
  | Require

  | UseStrict

  | ArithOp of AST_generic.operator
  (* less: should be in statement and unsugared in x+=1 or even x = x + 1 *)
  | IncrDecr of (AST_generic.incr_decr * AST_generic.prefix_postfix)
 [@@deriving show { with_path = false} ] (* with tarzan *)

type label = string wrap
 [@@deriving show ] (* with tarzan *)

(* the filename is not "resolved".
 * alt: use a reference like for resolved_name set in graph_code_js.ml and
 * module_path_js.ml? *)
type filename = string wrap
 [@@deriving show ] (* with tarzan *)

(* when doing export default Foo and import Bar, ... *)
let default_entity = "!default!"

type property_name = 
  (* this can even be a string or number *)
  | PN of ident
  (* especially useful for array objects, but also used for dynamic fields *)
  | PN_Computed of expr
  (* less: Prototype *)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
and expr =
  | Bool of bool wrap
  | Num of string wrap
  | String of string wrap
  | Regexp of string wrap

  (* For Global the ref is set after ast_js_build in a naming phase in 
   * graph_code_js, hence the use of a ref.
   *)
  | Id of ident * resolved_name ref 
  | IdSpecial of special wrap
  (* old: we used to have a Nop, without any token attached, which allowed
   * to simplify a bit the AST by replacing some 'expr option' into simply
   * 'expr' (for v_init, ForClassic, Return) but this bited us in the long term
   * in semgrep where we don't want metavariables to match code that does 
   * not exist.
   *)

  (* should be a statement, lhs can be a pattern *)
  | Assign of pattern * tok * expr

  (* less: could be transformed in a series of Assign(ObjAccess, ...) *)
  | Obj of obj_ 
  (* we could transform it in an Obj but can be useful to remember 
   * the difference in further analysis (e.g., in the abstract interpreter).
   * This can also contain "holes" when the array is used in lhs of an assign
   *)
  | Arr of expr list bracket
  | Class of class_ * ident option (* when assigned in module.exports  *)

  | ObjAccess of expr * tok * property_name
  (* this can also be used to access object fields dynamically *)
  | ArrAccess of expr * expr

  | Fun of fun_ * ident option (*when recursive or assigned in module.exports*)
  | Apply of expr * arguments

  (* copy-paste of AST_generic.xml (but with different 'expr') *)
  | Xml of xml

  (* could unify with Apply, but need Lazy special then *)
  | Conditional of expr * expr * expr

  (* sgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket

  and arguments = argument list bracket
   and argument = expr

    (* transpiled: to regular Calls when Ast_js_build.transpile_xml *)
    and xml = {
      xml_tag: ident; (* this can be "" for React "fragment", <>xxx</> *)
      xml_attrs: xml_attribute list;
      xml_body: xml_body list;
    }
     and xml_attribute = 
       | XmlAttr of ident * xml_attr_value
       (* jsx: usually a Spread operation, e.g., <foo {...bar} /> *)
       | XmlAttrExpr of expr bracket
       and xml_attr_value = expr
     and xml_body =
      | XmlText of string wrap
      | XmlExpr of expr
      | XmlXml of xml

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = 
  (* covers also method definitions, class defs, etc. Really a DefStmt *)
  | VarDecl of var

  | Block of stmt list bracket
  | ExprStmt of expr * tok (* can be fake when ASI *)
  (* todo? EmptyStmt of tok *)

  | If of tok * expr * stmt * stmt option
  | Do of tok * stmt * expr | While of tok * expr * stmt
  | For of tok * for_header * stmt

  | Switch of tok * expr * case list
  | Continue of tok * label option | Break of tok * label option
  | Return of tok * expr option

  | Label of label * stmt
 
  | Throw of tok * expr
  | Try of tok * stmt * catch option * (tok * stmt) option
  | With of tok * expr * stmt

  (* less: ModuleDirective of module_directive 
   * ES6 modules can appear only at the toplevel
  *  but CommonJS require() can be inside ifs
  *)
  | M of module_directive

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

  and case = 
   | Case of tok * expr * stmt
   | Default of tok * stmt

  and catch =
   | BoundCatch of tok * pattern * stmt
   (* js-ext: es2019 *)
   | UnboundCatch of tok * stmt

(*****************************************************************************)
(* Pattern (destructuring binding) *)
(*****************************************************************************)
(* reuse Obj, Arr, etc.
 * transpiled: to regular assignments when Ast_js_build.transpile_pattern.
 * sgrep: this is useful for sgrep to keep the ability to match over
 * JS destructuring patterns.
 *)
and pattern = expr

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
and var = { 
  (* ugly: can be AST_generic.special_multivardef_pattern when
   * Ast_js_build.transpile_pattern is false with a vinit an Assign itself.
   * actually in a ForIn/ForOf the init will be just the pattern, not even
   * an Assign.
   *)
  v_name: ident;
  v_kind: var_kind wrap;
  (* actually a pattern when inside a ForIn/ForOf *)
  v_init: expr option;
  v_resolved: resolved_name ref;
}
  and var_kind = Var | Let | Const

and fun_ = {
  f_props: fun_prop wrap list;
  f_params: parameter list;
  f_body: stmt;
}
  and parameter =
   | ParamClassic of parameter_classic
   (* transpiled: when Ast_js_build.transpile_pattern *)
   | ParamPattern of pattern
   (* sgrep-ext: *)
   | ParamEllipsis of tok
  and parameter_classic = {
    p_name: ident;
    p_default: expr option;
    p_dots: tok option;
  }
  (* less: could transpile *)
  and fun_prop = 
    | Generator | Async
    (* only inside classes *)
    | Get | Set 

and obj_ = property list bracket

and class_ = { 
  c_tok: tok;
  (* usually simply an Id *)
  c_extends: expr option;
  c_body: property list bracket;
}
  and property = 
    (* expr is a Fun for methods 
     * None is possible only for class fields. For object there is
     * always a value.
     *)
    | Field of property_name * property_prop wrap list * expr option
    (* less: can unsugar? *)
    | FieldSpread of tok * expr
    (* This is present only when in pattern context.
     * ugly: we should have a clean separate pattern type instead of abusing
     *  expr, which forces us to add this construct.
     *)
    | FieldPatDefault of pattern * tok * expr
    (* sgrep-ext: used for {fld1: 1, ... } which is distinct from spreading *)
    | FieldEllipsis of tok

  and property_prop =
    | Static
    (* todo? not in tree-sitter-js *)
    | Public | Private | Protected

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
  | Import of tok * ident * ident option (* 'name1 as name2' *) * filename
  | Export of tok * ident
  (* export * from 'foo' *)
  | ReExportNamespace of tok * tok * tok * filename

  (* hard to unsugar in Import because we do not have the list of names *)
  | ModuleAlias of tok * ident * filename (* import * as 'name' from 'file' *)

  | ImportCss of tok * filename
  (* those should not exist (except for sgrep where they are useful) *)
  | ImportEffect of tok * filename

  [@@deriving show { with_path = false} ]

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
(* this used to be a special type with only var, stmt, or module_directive
 * but tree-sitter allows module directives at stmt level, and anyway
 * we don't enforce those constraints on the generic AST so simpler to
 * move those at the stmt level.
 *)
type toplevel = stmt
 [@@deriving show { with_path = false} ] (* with tarzan *)

(*****************************************************************************)
(* Program *)
(*****************************************************************************)

type program = toplevel list
 [@@deriving show { with_path = false} ] (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any = 
  | Expr of expr
  | Stmt of stmt
  | Pattern of pattern
  | Item of toplevel
  | Items of toplevel list
  | Program of program

 [@@deriving show { with_path = false} ] (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let str_of_name (s, _) = s
let tok_of_name (_, tok) = tok

let unwrap x = fst x

and string_of_xhp_tag s = s

let mk_const_var id e = 
  { v_name = id; v_kind = Const, (snd id); v_init = Some e; 
    v_resolved = ref NotResolved }

(* helpers used in ast_js_build.ml and Parse_javascript_tree_sitter.ml *)
let var_pattern_to_var vkind pat tok init_opt = 
  let s = AST_generic.special_multivardef_pattern in
  let id = s, tok in
  let init = 
    match init_opt with
    | Some init -> Assign (pat, tok, init) 
    | None -> pat
  in
  (* less: use x.vpat_type *)
  {v_name = id; v_kind = vkind; v_init = Some init;
    v_resolved = ref NotResolved}

let special_of_id_opt s =
  match s with
  | "eval" -> Some Eval
  | "undefined" -> Some Undefined
  (* commonJS *)
  | "require"   -> Some Require
  | "exports"   -> Some Exports
  | "module"   -> Some Module
  (* AMD *)
  | "define"   -> Some Define
  (* reflection *)
  | "arguments"   -> Some Arguments
  | _ -> None

(* note that this should be avoided as much as possible for sgrep, because
 * what was before a simple sequence of stmts in the same block can suddently
 * be in different blocks.
 * Use stmt_item_list when you can in ast_js_build.ml
 *)
and stmt_of_stmts xs = 
  match xs with
  | [] -> Block (AST_generic.fake_bracket [])
  | [x] -> x
  | xs -> Block (AST_generic.fake_bracket xs)

let mk_default_entity_var tok exp = 
  let n = default_entity, tok in
  let v = { v_name = n; v_kind = Const, tok; v_init = Some exp; 
            v_resolved = ref NotResolved } in
  v, n
