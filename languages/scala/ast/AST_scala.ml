(* Yoann Padioleau
 *
 * Copyright (C) 2021-2022 R2C
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
(* A Concrete/Abstract Syntax Tree for Scala 2.
 *
 * I tried to keep the names used in the original Scala parser for
 * the AST constructs (e.g., Template for class/traits/objects, PatBind
 * for what I usually call PatAs, Apply for Call, bindings for parameters,
 * PatApply for Constructor, etc.),
 * or corresponding grammar rules (e.g., block_stat, block_expr, import_expr).
 * In case I didn't, I used the ast_orig: tag to indicate what was the
 * original name.
 *
 * See the scala3: tag for possible extensions to handle Scala 3.
 *
 * alt:
 * - mimic the AST types/classes in the Scala compiler, but they look
 *   very weakly typed (not as bad as just Node/Leaves, but not
 *   super precise either)
 * - use the Tasty format?
 *   https://github.com/lampepfl/dotty/blob/master/tasty/src/dotty/tools/tasty/TastyFormat.scala
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

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)
(* can be a regular ident (e.g., "foo") or an operator (e.g., "**") or
 * even a backquoted ident (e.g., `foo is great`).
 *)
type ident = string wrap [@@deriving show]

(* just used for prefixExpr *)
type op = string wrap [@@deriving show]

(* just for patterns, start with a lowercase letter *)
type varid = string wrap [@@deriving show]

let wildcard = "_"
let this = "this"

(* less: right now abusing ident to represent "_" *)
type ident_or_wildcard = ident [@@deriving show]
type varid_or_wildcard = ident [@@deriving show]

(* less: right now abusing ident to represent "this" *)
type ident_or_this = ident [@@deriving show]
type dotted_ident = ident list [@@deriving show]

(* just for packages for now *)
type qualified_ident = dotted_ident [@@deriving show]
type selectors = dotted_ident [@@deriving show]

(* scala3: not defined in scala2, but good name *)
type simple_ref =
  | Id of ident
  | This of ident option * tok (* 'this' *)
  | Super of ident option * tok (* 'super'*) * ident bracket option * ident
[@@deriving show { with_path = false }]

(* the dotted_ident can be empty *)
type path = simple_ref * selectors [@@deriving show]

(* A stable identifier is a path which ends in an identifier
 * src: https://scala-lang.org/files/archive/spec/2.13/03-types.html
 * This ending identifier can also contain be the special ident 'type'
 * when used in TyName
 *)
type stable_id = path [@@deriving show]

(*****************************************************************************)
(* Directives *)
(*****************************************************************************)

type import_path_elem =
  | ImportId of ident
  | ImportThis of tok
  | ImportSuper of tok
[@@deriving show]

type import_path = import_path_elem list [@@deriving show]

type named_selector = import_path_elem * ident_or_wildcard option
and wildcard_selector = (ident, tok * type_ option) Either_.t

and import_selector =
  | NamedSelector of named_selector
  | WildCardSelector of wildcard_selector

(* semgrep-ext: we allow single identifiers here so we can support import $X *)
and import_expr =
  | ImportExprSpec of import_path * import_spec
  | ImportExprMvar of ident

and import_spec =
  | ImportNamed of named_selector
  | ImportWildcard of wildcard_selector
  | ImportSelectors of import_selector list bracket

and import = tok (* 'import' *) * import_expr list
and package = tok (* 'package' *) * qualified_ident

(*****************************************************************************)
(* Start of big recursive type *)
(*****************************************************************************)
(* type_ and pattern_ used to not be mutually recursive, but once you
 * add interpolated strings in literal, then everything is mutually recursive.
 * scala3: literal is split in simple_literal and literal in which case
 * at least types can be defined independently of the other types
 * (but pattern can not).
 *)

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)
(* todo: interpolated strings? can be a literal pattern too?
 * scala3: called simple_literal
 *)
and literal =
  | Int of Parsed_int.t
  | Float of float option wrap
  | Char of string wrap
  | String of string wrap (* TODO: bracket *)
  | Bool of bool wrap
  | Symbol of tok (* "'" *) * string wrap
  (* scala3: not in simple_literal *)
  | Null of tok
  (* this forces to define type_ and pattern and expr as mutually recursive*)
  | Interpolated of ident (* e.g., s"..." *) * encaps list * tok (* " or """ *)

and encaps =
  | EncapsStr of string wrap
  | EncapsDollarIdent of ident (* e.g., (fst ident) does not contain $ *)
  (* 'expr' here! hence the big mutual recursive types below *)
  | EncapsExpr of expr (* will always be a BlockExpr with ${ ... } *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
and type_ =
  (* scala3: simple_literal, ast_orig: SingletonType *)
  | TyLiteral of literal (* crazy? genius? *)
  | TyName of stable_id
  (* ast_orig: SelectFromType *)
  | TyProj of type_ * tok (* '#' *) * ident
  (* ast_orig: AppliedType *)
  | TyApplied of type_ * type_ list bracket
  | TyAnon of tok (* '?' *) * type_bounds
  | TyInfix of type_ * ident * type_
  | TyFunction1 of type_ * tok (* '=>' *) * type_
  | TyFunction2 of type_ list bracket * tok (* '=>' *) * type_
  (* https://docs.scala-lang.org/scala3/reference/new-types/polymorphic-function-types.html *)
  | TyPoly of binding list * tok (* '=>' *) * type_
  (* https://docs.scala-lang.org/scala3/reference/new-types/dependent-function-types.html *)
  | TyDependent of (ident * type_) list * tok (* '=>' *) * type_
  | TyTuple of type_ list bracket
  | TyRepeated of type_ * tok (* '*' *)
  | TyByName of tok (* => *) * type_
  | TyAnnotated of type_ * annotation list (* at least one *)
  | TyRefined of type_ option * refinement
  | TyMatch of type_ * tok (* match *) * type_case_clauses
  | TyExistential of type_ * tok (* 'forSome' *) * refinement
  | TyWith of type_ * tok (* 'with' *) * type_
  | TyWildcard of tok (* '_' *) * type_bounds

and refinement = refine_stat list bracket

(* just dcls and type defs for TyRefined,
 * and val and type defs for TyExistential *)
and refine_stat = definition

and type_bounds = {
  supertype : (tok (* >: *) * type_) option;
  subtype : (tok (* <: *) * type_) option;
}

(* todo: also _* or annotation list *)
and ascription = type_

(*****************************************************************************)
(* Patterns *)
(*****************************************************************************)
and pattern =
  (* interpolated strings serve as regexp-like patterns (nice) *)
  | PatLiteral of literal
  | PatName of stable_id
  | PatTuple of pattern list bracket
  | PatVarid of varid_or_wildcard
  (* ast_orig: just Typed *)
  | PatTypedVarid of varid_or_wildcard * tok (* : *) * type_
  | PatBind of varid * tok (* @ *) * pattern
  (* less: the last pattern one can be '[varid @] _ *'
   * ast_orig: AppliedType for the type_ list bracket
   * less: could remove PatName and use PatApply (name, None, None).
   *)
  | PatApply of
      stable_id * type_ list bracket option * pattern list bracket option
  | PatInfix of pattern * ident * pattern
  (* less: only last element of a pattern list? *)
  | PatUnderscoreStar of tok (* '_' *) * tok (* '*' *)
  | PatDisj of pattern * tok (* | *) * pattern
  | PatQuoted of quoted
  (* semgrep-ext: *)
  | PatEllipsis of tok

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
and expr =
  | L of literal
  | Tuple of expr list bracket
  (* this path can be just 'this' *)
  | Name of path
  | ExprUnderscore of tok (* '_' *)
  (* ast_orig: TypeApply *)
  | InstanciatedExpr of expr * type_ list bracket (* ex: empty[List[Int]]? *)
  | TypedExpr of expr * tok (* : *) * ascription
  (* !TAKE CARE! (Name path) is also a disguised DotAccess *)
  | DotAccess of expr * tok (* . *) * ident
  (* in Scala you can have multiple argument lists! This is
   * used in Scala for ArrAccess, implicits, block as last argument, etc.
   *)
  | Apply of expr * arguments list
  (* in Scala any identifier can be used in infix position
   * (nice but also easy to abuse).
   * scala3: restricted to functions declared as 'infix'
   *)
  | Infix of expr * ident * expr
  (* ast_orig: converted as a Select *)
  | Prefix of op (* just -/+/~/! *) * expr
  | Postfix of expr * ident
  | Assign of lhs * tok (* = *) * expr
  | Match of expr * tok (* 'match' *) * case_clauses bracket
  | Lambda of function_definition
  | New of tok * template_definition
  | Quoted of quoted
  | BlockExpr of block_expr
  | S of stmt
  (* semgrep-ext: *)
  | Ellipsis of tok
  | DeepEllipsis of expr bracket
  | DotAccessEllipsis of expr * tok (* ... *)

(* only Name, or DotAccess, or Apply! (e.g., for ArrAccess) *)
and lhs = expr

(* represents: ParArgumentExprs, ArgumentExprs *)
and arguments =
  | Args of argument list bracket
  | ArgUsing of argument list bracket
  (* Ruby-style last argument used as a block (nice when defining your
   * own control structure) *)
  | ArgBlock of block_expr
(* less: no keyword argument in Scala? *)

and argument = expr
and case_clauses = (pattern, block) case_clause list
and type_case_clauses = (((* _ *) tok, type_) Either_.t, type_) case_clause list

and ('a, 'b) case_clause =
  | CC of ('a, 'b) case_clause_classic
  (* semgrep-ext: *)
  | CaseEllipsis of tok

and ('a, 'b) case_clause_classic = {
  casetoks : tok (* 'case' *) * tok (* '=>' *);
  case_left : 'a;
  caseguard : guard option;
  case_right : 'b;
}

and guard = tok (* 'if' *) * expr
and block_expr = block_expr_kind bracket
and block_expr_kind = BEBlock of block | BECases of case_clauses

and quoted =
  | QuotedBlock of tok * block bracket
  | QuotedType of tok * type_ bracket

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)
(* Note that in Scala everything is an expr, but I still like to split expr
 * with the different "subtype" 'stmt'. In some languages, e.g., Ruby, I
 * also put 'definition' as a "subtype" but in Scala we can restrict
 * them to appear only in block_stat (see block_stat below).
 *)
and stmt =
  | Block of block bracket
  | If of tok * expr bracket * expr * (tok * expr) option
  | While of tok * expr bracket * expr
  | DoWhile of tok * expr * tok * expr bracket
  | For of tok * for_header * for_body
  | Return of tok * expr option
  | Try of tok * expr * catch_clause option * finally_clause option
  | Throw of tok * expr

(* TODO: ForEllipsis for semgrep *)
and for_header = enumerators bracket

(* the first one is always a generator *)
and enumerators = enumerator list

and enumerator =
  | G of generator
  (* semgrep-ext: *)
  | GEllipsis of tok
(* less: GAssign *)

and generator = {
  genpat : pattern;
  gentok : tok; (* <- or = *)
  genbody : expr;
  genguards : guard list;
}

and for_body = Yield of tok * expr | NoYield of expr
and catch_clause = tok (* 'catch' *) * catch_body
and catch_body = CatchCases of case_clauses bracket | CatchExpr of expr
and finally_clause = tok (* 'finally' *) * expr

(*****************************************************************************)
(* XxxStats *)
(*****************************************************************************)
(* less: the last can be a ResultExpr *)
and block = block_stat list

(* pad: not sure what Stat means in original grammar. Statement? *)
and block_stat =
  | D of definition
  | I of import
  | Ex of import
  | Ext of extension
  | End of end_marker
  | E of expr
  (* just at the beginning of top_stat *)
  | Package of package
  | Packaging of package * top_stat list bracket

(* those have special restrictions but simpler to make them alias
 * to block_stat. Anyway in AST_generic they will be all converted
 * to stmts/items.
 *)
and template_stat = block_stat
and top_stat = block_stat

(*****************************************************************************)
(* Attributes (modifiers and annotations) *)
(*****************************************************************************)
and modifier = modifier_kind wrap

and modifier_kind =
  (* local modifier *)
  | Abstract
  | Final
  (* scala specific *)
  | Sealed
  | Implicit
  | Lazy
  (* access modifier *)
  | Private of ident_or_this bracket option
  | Protected of ident_or_this bracket option
  (* misc (and nice!) *)
  | Override
  | Inline
  | Open
  | Opaque
  (* pad: not in original spec *)
  | CaseClassOrObject
  (* less: rewrite as Packaging and object def like in original code? *)
  | PackageObject
  (* just for variables/fields/class params *)
  | Val (* immutable *)
  | Var (* mutable *)
  | EnumClass (* for enum classes *)

and annotation = tok (* @ *) * type_ (* usually just a TyName*) * arguments list
and attribute = A of annotation | M of modifier

(*****************************************************************************)
(* Type parameter (generics) *)
(*****************************************************************************)
(* I'm using the same type for type parameters for classes and functions
 * but variance constructs apply only for classes.
 *)
and type_parameter = {
  tpname : ident_or_wildcard;
  tpvariance : variance wrap option;
  tpannots : annotation list;
  (* wow, this is complicated *)
  tpparams : type_parameters option;
  tpbounds : type_bounds;
  tpviewbounds : (* <% *) type_ list;
  tpcolons : (* : *) type_ list;
}

and variance =
  | Covariant
  (* + *)
  | Contravariant (* - *)

and type_parameters = type_parameter list bracket

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* definition or declaration (def or dcl) *)
and definition =
  | DefEnt of entity * definition_kind
  | EnumCaseDef of attribute list * enum_case_definition
  | GivenDef of given_definition
  (* note that some VarDefs are really disgused FuncDef when
   * the vbody is a BECases
   *)
  | VarDefs of variable_definitions

(* ------------------------------------------------------------------------- *)
(* Val/Var entities *)
(* ------------------------------------------------------------------------- *)
(* Used for local variables but also for fields *)
and variable_definitions = {
  (* a bit like entity, but for a list of stuff because of the pattern *)
  vpatterns : pattern list;
  vattrs : attribute list;
  (* old: vkind: variable_kind wrap;, now in vattrs *)
  vtype : type_ option;
  vbody : expr option; (* None for declarations *)
}

(* ------------------------------------------------------------------------- *)
(* Other entities *)
(* ------------------------------------------------------------------------- *)
and entity = {
  (* can be "this" for constructor *)
  name : ident;
  attrs : attribute list;
  tparams : type_parameters option;
}

(* less: also work for declaration, in which case the [fc]body is empty *)
and definition_kind =
  | FuncDef of function_definition
  | TypeDef of type_definition
  (* class/traits/objects *)
  | Template of template_definition

(*****************************************************************************)
(* End Marker *)
(*****************************************************************************)
and end_marker = { end_tok : tok; end_kind : tok }

(*****************************************************************************)
(* Extensions *)
(*****************************************************************************)
and extension = {
  ext_tok : tok; (* extension *)
  ext_tparams : type_parameters option;
  ext_using : bindings list;
  ext_param : binding;
  ext_methods : ext_method list;
}

and ext_method = ExtDef of definition | ExtExport of import

(* ------------------------------------------------------------------------- *)
(* Enums *)
(* ------------------------------------------------------------------------- *)
and enum_case_definition = EnumConstr of enum_constr | EnumIds of ident list

and enum_constr = {
  eid : ident;
  etyparams : type_parameters option;
  eparams : bindings list;
  eattrs : attribute list;
  eextends : constr_app list;
}

(* annotations built into type *)
and constr_app = type_ * arguments list

(* ------------------------------------------------------------------------- *)
(* Functions/Methods *)
(* ------------------------------------------------------------------------- *)
and function_definition = {
  fkind : function_kind wrap;
  (* a list of list of parameters! but usually 0/1/2 *)
  fparams : bindings list;
  (* scala3? remove None and force : Unit ? *)
  frettype : type_ option;
  fbody : fbody option; (* None for declarations *)
}

and function_kind =
  | LambdaArrow
  (* '=>' *)
  | Def
(* 'def' *)
(* less: Constructor, when name = "this"? *)

and fbody =
  | FBlock of block_expr
  | FExpr of tok (* = (or => for lambdas) *) * expr

(* fake brackets for single param in short lambdas *)
and bindings = (binding list * tok option (* using *)) bracket

and binding =
  | ParamClassic of parameter_classic
  | ParamType of type_
  (* semgrep-ext: *)
  | ParamEllipsis of tok

and parameter_classic = {
  p_name : ident_or_wildcard;
  (* especially var/val, and implicit *)
  p_attrs : attribute list;
  (* None only in Lambdas; Def must define types for each parameters *)
  p_type : param_type option;
  p_default : expr option;
}

and param_type =
  | PT of type_
  | PTByNameApplication of tok (* => *) * type_ * (* * *) tok option
  | PTRepeatedApplication of type_ * tok (* * *)

(* ------------------------------------------------------------------------- *)
(* Traits/Classes/Objects *)
(* ------------------------------------------------------------------------- *)

(* =~ class def, hence the c prefix below.
 * Note that this is also used for New, in which case
 * the constructor arguments are passed via cextends.
 *)
and template_definition = {
  ckind : template_kind wrap;
  (* also a list of list of parameters *)
  cparams : bindings list;
  cparents : template_parents;
  cbody : template_body option;
}

(* scala3: intersection types so more symetric *)
and template_parents = {
  cextends : (type_ * arguments list) option;
  cwith : type_ list;
}

and template_body = (self_type option * block) bracket

(* if use this then type_ can't be None *)
and self_type = ident_or_this * type_ option * tok (* '=>' *)

(* Case classes/objects are handled via attributes in the entity *)
and template_kind =
  | Class
  | Trait
  | Object
  | Singleton
  (* via new *)
  | Enum

(* ------------------------------------------------------------------------- *)
(* Given definitions *)
(* ------------------------------------------------------------------------- *)
and given_sig = {
  g_id : ident option;
  g_tparams : type_parameters option;
  g_using : bindings list;
  g_colon : tok;
}

and given_kind =
  | GivenStructural of constr_app list * template_body option
  (* combination of both alias instance and abstract instance *)
  | GivenType of type_ * expr option

and given_definition = { gsig : given_sig option; gkind : given_kind }

(* ------------------------------------------------------------------------- *)
(* Typedef *)
(* ------------------------------------------------------------------------- *)
and type_definition = { ttok : tok; (* 'type' *) tbody : type_definition_kind }

and type_definition_kind = TDef of tok (* = *) * type_ | TDcl of type_bounds
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Toplevel elements *)
(*****************************************************************************)

type program = top_stat list [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any = Pr of program | Ex of expr | Ss of block | Tk of tok
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let empty_cparents = { cextends = None; cwith = [] }
let attrs_of_mods xs = List_.map (fun x -> M x) xs
let attrs_of_annots xs = List_.map (fun x -> A x) xs
let mods_with_annots mods annots = attrs_of_annots annots @ attrs_of_mods mods

let is_variable_name s =
  (* start with lowercase, see varid *)
  s =~ "[_ a-z].*"

let basic_param id =
  { p_name = id; p_type = None; p_attrs = []; p_default = None }
