(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A generic AST, to factorize similar analysis in different programming
 * languages (e.g., naming, semantic code highlighting, semgrep matching).
 *
 * !!!If you modify this file, please adjust the 'version' variable below!!!
 *
 * Right now this generic AST is mostly the factorized union of:
 *  - Python, Ruby, Lua, Julia, Elixir
 *  - Javascript, Typescript, Vue
 *  - PHP, Hack
 *  - Java, C#, Kotlin
 *  - C, C++
 *  - Go
 *  - Swift
 *  - OCaml, Scala, Rust
 *  - Clojure, Lisp, Scheme
 *  - R
 *  - Solidity
 *  - Bash, Docker
 *  - JSON, XML, YAML
 *  - Jsonnet, Terraform
 *  - HTML
 *  - TODO SQL, Sqlite, PostgresSQL
 *
 * See Lang.ml for the list of supported languages.
 * See IL.ml for a generic IL (Intermediate language) better suited for
 * advanced static analysis (e.g., tainted dataflow analysis).
 *
 * rational: In the end, programming languages have a lot in Common.
 * Even though some interesting analysis are probably better done on a
 * per-language basis, many analysis are simple and require just an
 * AST and a visitor. One could duplicate those analysis for each language
 * or design an AST (this file) generic enough to factorize all those
 * analysis. Note that we want to remain
 * as precise as possible and not lose too much information while going
 * from the specific language AST to the generic AST. We don't want
 * to be too generic as in ast_fuzzy.ml (or Raw_tree.ml), where we have a
 * very general tree of nodes, but all the structure of the original AST is
 * lost.
 *
 * The generic AST tries to be as close as possible to the original code but
 * not too close. When a programming language feature is really sugar or
 * an alternative way to do a thing, we usually unsugar. Here are the
 * simplifications done:
 *  - we do not keep the comma tokens in arguments. More generally we
 *    just keep the tokens to get the range right (see the discussions on
 *    invariants below) and get rid of the other (e.g., we remove
 *    parens around conditions in if). We keep the parens for 'Call'
 *    because we want to get the right range for those so we need the
 *    rightmost tokens in the AST.
 *  - multiple var declarations in one declaration (e.g., int a,b; in C)
 *    are expanded in multiple 'variable_definition'. Note that
 *    tuple assignments (e.g., a,b=1,2) are not expanded in multiple assigns
 *    because this is not always possible (e.g., a,b=foo()) and people may
 *    want to explicitely match tuples assignments (we do some magic in
 *    Generic_vs_generic though to let 'a=1' matches also 'a,b=1,2').
 *  - multiple entity imports in one declaration (e.g., from foo import {a,b})
 *    are expanded in multiple individual imports
 *    (in the example, from foo import a; from foo import b).
 *    update: we don't expand them anymore
 *  - multiple ways to define a function are converted all to a
 *    'function_definition' (e.g., Javascript arrows are converted in that)
 *    update: but we now have a more precise function_body type
 *  - we are more general and impose less restrictions on where certain
 *    constructs can appear to simplify things.
 *     * there is no special lhs/lvalue type (see IL.ml for that) and so
 *      'Assign' takes a general 'expr' on its lhs.
 *     * there is no special toplevel/item vs stmt. Certain programming
 *       languages impose restrictions on where a function or directive can
 *       appear (e.g., just at the toplevel), but we allow those constructs
 *       at the stmt level.
 *     * the Splat and HashSplat operator can usually appear just in arguments
 *       or inside arrays or in struct definitions (a field) but we are more
 *       general and put it at the 'expr' level.
 *     * certain attributes are valid only for certain constructs but instead
 *       we use one attribute type (no class_attribute vs func_attribute etc.)
 *
 * Note that this generic AST has become gradually more and more a
 * generic CST, to fix issues in autofix in Semgrep.
 * TODO? it may be time to rename this file CST_generic.ml
 *
 * todo:
 *  - improve things for Kotlin/Scala/Rust/C++/Java
 *
 * related work:
 *  - lib_parsing/ast_fuzzy.ml
 *  - spacegrep and aliengrep of Martin with a general Pat_AST.ml
 *  - github semantic (seems dead)
 *    https://github.com/github/semantic
 *  - UAST of babelfish
 *    https://doc.bblf.sh/uast/uast-specification-v2.html
 *  - Coverity common program representation?
 *  - Semmle internal common representation?
 *  - Sonarcube generic language
 *    https://github.com/SonarSource/slang
 *  - Facebook Infer SIL (for C++, Java, Objective-C)
 *  - Dawson Engler and Fraser Brown micro-checkers for multiple languages
 *  - Comby common representation by Rijnard,
 *    see "Lightweight Multi-language syntax transformation", but it does not
 *    really operate on an AST
 *  - https://tabnine.com/ which supports multiple languages, but probably
 *    again does not operate on an AST
 *  - srcML https://www.srcml.org/doc/srcMLGrammar.html
 *    but just for C/C++/C#/Java and seems pretty heavy
 *
 * design choices to have a generic data structure:
 *  - add some 'a, 'b, 'c around expr/stmt/...
 *  - data-type a la carte like in github-semantic but IMHO too high-level
 *    with astronaut-style architecture (too abstract, too advanced features).
 *  - CURRENT SOLUTION: the OtherXxx strategy used in this file (simple)
 *    update: actually switch to OtherXxx of todo_kind, even simpler
 *  - functorize and add some type hole (type tstmt; type texpr; ...),
 *    todo? not a bad idea if later we want to add type information on each
 *    expression nodes
 *
 * history:
 *  - started with crossproduct of Javascript, Python, PHP, Java, and C
 *    (and a bit of OCaml) after wanting to port checked_return from Js to
 *    Python and got the idea to factorize things
 *
 * INVARIANTS:
 *  - all the other_xxx types should contain only simple constructors (enums)
 *    without any parameter. I rely on that to simplify the code
 *    of the generic mapper and matcher.
 *    update: prefer todo_kind to those other_xxx types now.
 *    Same for keyword_attributes.
 *  - each expression or statement must have at least one token in it
 *    so that semgrep can track a location (e.g., 'Return of expr option'
 *    is not enough because with no expr, there is no location information
 *    for this return, so it must be 'Return of tok * expr option' instead)
 *  - each expression or statement should ideally have enough tokens in it
 *    to get its range, so at least the leftmost and rightmost token in
 *    all constructs, so the Return above should even be
 *    'Return of tok * expr option * tok' for the ending semicolon
 *    (alt: have the range info in each expr/stmt/pattern but big refactoring)
 *  - to correctly compute a CFG (Control Flow Graph), the stmt type
 *    should list all constructs that contains other statements and
 *    try to avoid to use the very generic 'OtherXxx of any'
 *  - to correctly compute a DFG (Data Flow Graph), and to correctly resolve
 *    names (see Naming_AST.ml), each constructs that introduce a new
 *    variable should have a relevant comment 'newvar:'
 *  - to correctly resolve names, each construct that introduces a new scope
 *    should have a relevant comment 'newscope:'
 *  - todo? each language should add the VarDefs that defines the locals
 *    used in a function (instead of having the first Assign play the role
 *    of a VarDef, as done in Python for example).
 *)

(* !! Modify version below each time you modify the generic AST!! There are
 * now a few places where we cache the generic AST in a marshalled binary
 * form on disk (e.g., in src/runner/Parsing_with_cache.ml) and reading back
 * old version of this AST can lead to segfaults in OCaml.
 * Note that this number below could be independent of the versioning scheme of
 * Semgrep; we don't have to update version below for each version of
 * Semgrep, just when we actually modify the generic AST. However it's convenient
 * to correspond mostly to Semgrep versions. So version below can jump from
 * "1.12.1" to "1.20.0" and that's fine.
 *)
let version = "1.19.0-2"

(* Provide hash_* and hash_fold_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

(* ppx_hash refuses to hash mutable fields but we do it anyway. *)
let hash_fold_ref hash_fold_x acc x = hash_fold_x acc !x

(*****************************************************************************)
(* Token (leaf) *)
(*****************************************************************************)
(* Contains among other things the position of the token through
 * the Tok.location embedded inside it, as well as the
 * transformation field that makes possible spatch on the code.
 *
 * Tok.t_always_equal is the same type as Tok.t but provides special equal and
 * hash functions that are more conveninent in Semgrep matching context.
 * See Matching_generic.equal_ast_bound_code() and Metavariable.equal_mvalue()
 * for more information.
 *)
type tok = Tok.t_always_equal [@@deriving show, eq, hash]

(* a shortcut to annotate some information with position information *)
type 'a wrap = 'a * tok [@@deriving show, eq, hash]

(* Use for round(), square[], curly{}, and angle<> brackets.
 * note: in theory we should not care about those tokens in an AST,
 * but they are useful to report correct ranges in sgrep when we match
 * something that can just be those brackets (e.g., an empty container).
 *)
type 'a bracket = tok * 'a * tok [@@deriving show, eq, hash]

(* semicolon, a FakeTok in languages that do not require them (e.g., Python).
 * alt: tok option.
 * See the sc value also at the end of this file to build an sc.
 *)
type sc = tok [@@deriving show, eq, hash]

(* an AST element not yet handled.
 * history: I started by having some precise OtherXxx of other_xxx
 * constructors and types to record what was not handled
 * (e.g., OE_Delete, OE_Define, etc.), but it was quickly getting tedious
 * each time to add new constructs. In fact, in the language-specific
 * ASTs I started to use also some Todo constructs, so I switched to a
 * more general todo_kind in the generic AST too. Anyway, we were
 * not doing anything with the precise information. If something
 * is important and require some semantic equivalence in semgrep, then
 * we should support the construct directly, not via an other_xxx.
 *)
type todo_kind = string wrap [@@deriving show, eq, hash]

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

type ident = string wrap [@@deriving show, eq, hash]

(* Usually separated by a '.', but can be used also with '::' separators.
 * less: we often need to get the last elt or adjust the qualifier part,
 * so maybe we should define it as = ident list * ident
 *)
type dotted_ident = ident list (* at least 1 element *)
[@@deriving show, eq, hash]

(* module_name can also be used for a package name or a namespace.
 * TODO? prefix with M and add MQualifiedName for C++?
 * less: can even be dynamic in C with #include of expr
 *)
type module_name =
  | DottedName of dotted_ident (* ex: Python *)
  (* in FileName the '/' is similar to the '.' in DottedName.
   * In C/C++ the string can be <foo.h>.
   *)
  | FileName of string wrap (* ex: Js import, C #include, Go import *)
[@@deriving show { with_path = false }, eq, hash]

(* OCaml has generative functors. This means the types `SId.t` and
   `IdInfoId.t` are different, even though both are represented by ints.
   This will help enforce that we don't do bad things with these ints by making
   them abstract.
*)
module SId = Gensym.MkId ()
module IdInfoId = Gensym.MkId ()

(* A single unique id: sid (uid would be a better name, but it usually
 * means "user id" for people).
 *
 * This single id simplifies further analysis that need to care less about
 * maintaining scoping information, for example to deal with variable
 * shadowing, or functions using the same parameter names
 * (even though you still need to handle specially recursive functions), etc.
 *
 * See Naming_AST.ml for more information.
 *
 * Most generic ASTs have a fake value (SId.unsafe_default at first.
 * You need to call Naming_AST.resolve (or one of the lang-specific
 * Resolve_xxx.resolve) on the generic AST to set it correctly.
 *)
(* a single unique gensym'ed number. *)
type sid = SId.t
and resolved_name = resolved_name_kind * sid

and resolved_name_kind =
  (* Global is useful in codemap/efuns to highlight differently and warn
   * about the use of globals inside functions.
   * old: Global was merged with ImportedEntity before but simpler to split, as
   * anyway I was putting often an empty list for dotted_ident with a
   * todo note in the code.
   *)
  | Global
  (* Those could be merged, but again this is useful in codemap/efuns *)
  | LocalVar
  | Parameter
  (* For closures; can refer to a Local or Param.
   * With sid this is potentially less useful for scoping-related issues,
   * but this can be useful in codemap to again highlight specially
   * enclosed vars.
   * todo: this is currently used also for fields, but we should use another
   * constructor.
   * Note that it's tempting to add a depth parameter to EnclosedVar, but
   * that would prevent semgrep to work because whatever the depth you are,
   * if you reference the same entity, this entity must have the same
   * resolved_name (sid and resolved_name_kind).
   *)
  | EnclosedVar (* less: add depth? *)
  (* sgrep: those cases allow to match entities/modules even if they were
   * aliased when imported.
   * both dotted_ident must at least contain one element *)
  | ImportedEntity of canonical_name
  | ImportedModule of
      canonical_name (* just the DottedName part of module_name*)
  (* used in Go, where you can pass types as arguments and where we
   * need to resolve those cases
   *)
  | TypeName
  (* used for C *)
  | Macro
  | EnumConstant
  (* This is for deep semgrep.
   *
   * GlobalName (canonical_name, alternate_names)
   *
   * canonical_name: The canonical, global name that the symbol resolves to.
   *
   * alternate_names: Other names that users may write when referring to this
   * symbol. For example, in JS, the canonical_name may include the file path
   * of the file where the symbol is defined, but users may want to write
   * patterns to match based on the module specifier, e.g.:
   *
   * import {bar} from 'foo';
   * bar;
   *
   * We might store ['/path/to/node_modules/foo/src/x.js', 'bar'] as the
   * canonical_name, but we also want to match the pattern `foo.bar` so we will
   * store ['foo', 'bar'] as an alternate name.
   * *)
  | GlobalName of canonical_name * alternate_name list

and canonical_name = string list

and alternate_name = string list
[@@deriving show { with_path = false }, eq, hash]

(* Used as a parent class for the autogenerated iter visitor, generated below
 * the large recursive type. *)
class virtual ['self] iter_parent =
  object (self : 'self)
    (* Virtual methods
     *
     * We could instead inherit from VisitorsRuntime.iter, which implements
     * these, but then we'd have to suppress warning 7 (since the methods would
     * be defined both here and in the generated visitor class which also
     * inherits from VisitorsRuntime.iter). For just a handful of such cases,
     * declaring virtual methods seems preferable. *)
    method virtual visit_string : 'env. 'env -> string -> unit

    method virtual visit_list
        : 'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit

    (* Handcoded visitor methods
     *
     * These are for nonlocal types. We could instead generate visitors at the
     * definition site and inherit those visitors here, but these are all simple
     * enough that for now it seems like it makes sense to just handcode them.
     * *)
    method visit_ident env id = self#visit_wrap self#visit_string env id

    method visit_bracket
        : 'a. ('env -> 'a -> unit) -> 'env -> 'a bracket -> unit =
      fun f env (left, x, right) ->
        self#visit_tok env left;
        f env x;
        self#visit_tok env right

    method visit_wrap : 'a. ('env -> 'a -> unit) -> 'env -> 'a wrap -> unit =
      fun f env (x, tok) ->
        f env x;
        self#visit_tok env tok

    method visit_todo_kind env kind = self#visit_wrap self#visit_string env kind

    (* This is a bit of gymnastics. Because Raw_tree.t is polymorphic, deriving
     * visitors generates a visit_raw_tree method for the monomorphic raw_tree
     * type, which expects there to be a polymorphic visit_raw_tree_t for `'a
     * Raw_tree.t`. That's a problem because child classes want to override
     * visit_raw_tree and intercept *every* raw_tree node, even nested ones. If
     * we allowed this to be a polymorphic method as expected, there would be no
     * way to call back into the monomorphic visit_raw_tree when visiting nested
     * `raw_tree`s.
     *
     * So, instead, we just declare the `visit_raw_tree` method as virtual
     * below (using type variables which will later get pinned down, since we
     * don't have access to the `raw_tree` type), and call it here.
     *
     * This makes this method monomorphic on the `Raw_tree.t` type parameter in
     * practice.
     *
     * If we introduced another use of `Raw_tree.t` in this big recursive type,
     * with a different type parameter, then we would start getting type errors
     * because OCaml would expect this method to be polymorphic. However, since
     * we will likely never use `Raw_tree.t` here with a type parameter other
     * than `AST_generic.any`, this doesn't matter. *)
    method visit_raw_tree_t f env x =
      Raw_tree.visit ~v_raw_tree:(self#visit_raw_tree env)
        ~v_token:(self#visit_wrap self#visit_string env)
        ~v_any:(f env) x

    (* See comment on visit_raw_tree_t. 'raw_tree will get pinned down in the
     * generated visitor to be `any Raw_tree.t`, but we can't refer to `any`
     * here so a type variable does the trick. *)
    method virtual visit_raw_tree : 'env -> 'raw_tree -> unit
    method visit_sc env tok = self#visit_tok env tok

    method visit_dotted_ident env dotted =
      self#visit_list self#visit_ident env dotted

    method visit_module_name env =
      function
      | DottedName dotted -> self#visit_dotted_ident env dotted
      | FileName fn -> self#visit_wrap self#visit_string env fn

    (* Stubs
     *
     * These are terminal types, or at least very uncomplicated ones which don't
     * contain tokens or anything else that is likely to be interesting to a
     * visitor. Subclasses can always override these with their own behavior if
     * needed. *)
    method visit_location _env _ = ()
    method visit_id_info_id_t _env _ = ()
    method visit_resolved_name _env _ = ()
    method visit_tok _env _ = ()
    method visit_node_id_t _env _ = ()
    method visit_string_set_t _env _ = ()
  end

(* Basically a copy paste of iter_parent above, but with different return types
 * *)
class virtual ['self] map_parent =
  object (self : 'self)
    (* Virtual methods *)
    method virtual visit_string : 'env. 'env -> string -> string

    method virtual visit_list
        : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method virtual visit_option
        : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    (* Handcoded visitor methods *)
    method visit_ident env id = self#visit_wrap self#visit_string env id

    method visit_bracket
        : 'a. ('env -> 'a -> 'a) -> 'env -> 'a bracket -> 'a bracket =
      fun f env (left, x, right) ->
        let left = self#visit_tok env left in
        let x = f env x in
        let right = self#visit_tok env right in
        (left, x, right)

    method visit_wrap : 'a. ('env -> 'a -> 'a) -> 'env -> 'a wrap -> 'a wrap =
      fun f env (x, tok) ->
        let x = f env x in
        let tok = self#visit_tok env tok in
        (x, tok)

    method visit_todo_kind env kind = self#visit_wrap self#visit_string env kind

    (* This is a bit fiddly. See the comment on visit_raw_tree_t in iter_parent
     * above. *)
    method visit_raw_tree_t f env x =
      (* TODO Generate or handcode this in Raw_tree.ml? *)
      Raw_tree.(
        match x with
        | Token wrapped -> Token (self#visit_wrap self#visit_string env wrapped)
        | List lst -> List (self#visit_list self#visit_raw_tree env lst)
        | Tuple lst -> Tuple (self#visit_list self#visit_raw_tree env lst)
        | Case (str, x) ->
            Case (self#visit_string env str, self#visit_raw_tree env x)
        | Option x -> Option (self#visit_option self#visit_raw_tree env x)
        | Any x -> Any (f env x))

    method virtual visit_raw_tree : 'env -> 'raw_tree -> 'raw_tree
    method visit_sc env tok = self#visit_tok env tok

    method visit_dotted_ident env dotted =
      self#visit_list self#visit_ident env dotted

    method visit_module_name env =
      function
      | DottedName dotted -> DottedName (self#visit_dotted_ident env dotted)
      | FileName fn -> FileName (self#visit_wrap self#visit_string env fn)

    (* Stubs *)
    method visit_location _env x = x
    method visit_id_info_id_t _env x = x
    method visit_resolved_name _env x = x
    method visit_tok _env x = x
    method visit_node_id_t _env x = x
    method visit_string_set_t _env x = x
  end

(* Start of big mutually recursive types because of the use of 'any'
 * in OtherXxx *)

(* old: Id below used to be called Name and was generalizing also IdQualified
 * but some analysis are easier when they just need to
 * handle a simple Id, hence the split. For example, there was some bugs
 * in sgrep because sometimes an identifier was an ident (in function header)
 * and sometimes a name (when called). For naming, we also need to do
 * things differently for Id vs IdQualified and would need many times to
 * inspect the name.name_qualifier to know if we have an Id or IdQualified.
 * We do the same split for Fid vs FName for fields.
 *
 * newvar: Id is sometimes abused to also introduce a newvar (as in Python)
 * but ultimately those cases should be rewritten to first introduce
 * a VarDef.
 *
 * DEBT? Sometimes some DotAccess should really be transformed in IdQualified
 * with a better qualifier because the obj is actually the name of a package
 * or module, but you may need advanced semantic information and global
 * analysis to disambiguate. In the meantime, you can use
 * AST_generic_helpers.name_of_dot_access to convert a DotAccess of idents
 * into an IdQualified name.
 *
 * sgrep-ext: note that ident can be a metavariable.
 *)
type name = Id of ident * id_info | IdQualified of qualified_info

(* A qualified (via type arguments or module/namespace/package) id.
 * The type should be enough to represent Java/Rust/C++ generics.
 * less: it is still not enough to represent OCaml functors applications.
 *
 * invariant: you can't have name_top = None, name_middle = QNone, and
 * name_last = (id * None) at the same time. If that's the case, then we
 * build an Id, not an Idqualified
 *)
and qualified_info = {
  name_last : ident * type_arguments option;
  name_middle : qualifier option;
  (* ::, Ruby, C++, also '`' abuse for PolyVariant in OCaml *)
  name_top : tok option;
  name_info : id_info;
}

and qualifier =
  (* Java/C++/Rust *)
  | QDots of (ident * type_arguments option) list
  (* Ruby/Lua *)
  | QExpr of expr * tok

(*****************************************************************************)
(* Naming/typing *)
(*****************************************************************************)
and id_info = {
  id_resolved : resolved_name option ref;
  (* variable tagger (naming) *)
  (* sgrep: in OCaml we also use that to store the type of
   * a typed entity, which can be interpreted as a TypedMetavar in semgrep.
   * alt: have an explicity type_ field in entity.
   *)
  id_type : type_ option ref;
  (* type checker (typing) *)
  (* sgrep: this is for sgrep constant propagation hack.
   * todo? associate only with Id?
   * note that we do not use the svalue for equality (hence the adhoc
   * @equal below) because the svalue analysis is now controlflow-sensitive
   * meaning the same variable might have different id_svalue value
   * depending where it is used.
   *)
  id_svalue : svalue option ref; [@equal fun _a _b -> true]
  (* THINK: Drop option? *)
  (* id_hidden=true must be set for any artificial identifier that never
     appears in source code but is introduced in the AST after parsing.

     Don't use this for syntax desugaring or transpilation because the
     resulting function name might exist in some source code. Consider the
     following normalization:

       !foo -> foo.contents
                   ^^^^^^^^
                 should not be marked as hidden because it could appear
                 in target source code.

     However, an artificial identifier like "!sh_quoted_expand!" should
     be marked as hidden in bash.

     This allows not breaking the -fast/-filter_irrelevant_rules optimization
     that skips a target file if some identifier in the pattern AST doesn't
     exist in the source of the target.
  *)
  id_hidden : bool;
  (* this is used by Naming_X in deep-semgrep *)
  id_info_id : id_info_id; [@equal fun _a _b -> true]
}

(* See explanation for @name where the visitors are generated at the end of this
 * long recursive type. *)
and id_info_id = (IdInfoId.t[@name "id_info_id_t"])

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* todo? we could store more semantic information at each expr node,
 * e.g., type information, constant evaluation.
 *)
and expr = {
  e : expr_kind;
  e_id : int;
  (* used to quickly get the range of an expression *)
  mutable e_range : (Tok.location * Tok.location) option;
      [@equal fun _a _b -> true] [@hash.ignore]
}

and expr_kind =
  (* basic (atomic) values *)
  | L of literal
  (* composite values
   * TODO? element list bracket, so can encode KeyVal, Designator,
   * IndexArray for C++.
   * ArrayInitDesignator [x] = ... use ArrayAccess in container?
   *)
  | Container of container_operator * expr list bracket
  | Comprehension of container_operator * comprehension bracket
  (* And-type (field.vinit should be a Some) *)
  | Record of field list bracket
  (* Or-type (could be used instead of Container, Cons, Nil, etc.).
   * (ab)used also for polymorphic variants where qualifier is QTop with
   * the '`' token.
   *
   * Note that in OCaml constructors do not always need brackets.
   * For example, you can have 'let x = Foo 1'. However, you often
   * need the parenthesis, for example when you have multiple arguments
   * (e.g., 'let x = Foo (1,2)').
   * In that case, we could make '(1,2)' a single Tuple argument, and
   * that is mostly what is done in ast_ml.ml, but this is a bit ugly.
   * Morever, even with a single argument, you need extra parenthesis
   * if the argument is a complex expression (e.g., 'let x = Foo(1+2)').
   * And if those parenthesis are not in the AST, then matching range
   * or autofix on such construct may fail.
   * Finally, in some languages like Scala or Rust, constructors require
   * the parenthesis.
   * Thus, it is simpler to add the bracket here, because this is mostly
   * how user think.
   *)
  | Constructor of name * expr list bracket
  (* Regexp templates are interpolated strings. Constant regexps aren't
     represented here but under 'literal' so as to benefit from constant
     propagation. This is meant to be similar to how string literals
     and string templates are handled or should be handled eventually.

     The current type allows any expr because it makes matching simpler.
     However, only the following fragment kinds are legitimate:
     - literal fragment
     - inserted expression
     - semgrep ellipsis (...)
     - semgrep metavariable ($X and $...X)
  *)
  | RegexpTemplate of expr bracket (* // *) * string wrap option (* modifiers *)
  (* see also New(...) for other values *)
  | N of name
  | IdSpecial of
      special wrap (*e: [[AST_generic.expr]] other identifier cases *)
  (* operators and function application *)
  | Call of expr * arguments
  (* 'type_' below is usually a TyN or TyArray (or TyExpr).
   * 'id_info' refers to the constructor.
   * Note that certain languages do not have a 'new' keyword
   * (e.g., Python, Scala 3), instead certain 'Call' are really 'New'.
   * old: this is used to be an IdSpecial used in conjunction with
   * Call ([ArgType _; ...]) but better to be more precise here as
   * New is really important for typing (and other program analysis).
   * note: see also AnonClass which is also a New.
   *)
  | New of tok (* 'new' (can be fake) *) * type_ * id_info * arguments
  (* TODO? Separate regular Calls from OpCalls where no need bracket and Arg *)
  (* (XHP, JSX, TSX), could be transpiled also (done in IL.ml?) *)
  | Xml of xml
  (* IntepolatedString of expr list is simulated with a
   * Call(IdSpecial (Concat ...)) *)
  (* The left part should be an lvalue (Id, DotAccess, ArrayAccess, Deref)
   * but it can also be a pattern (Container, even Record), but
   * you should really use LetPattern for that.
   *
   * newvar: Assign is also sometimes abused to declare new variables
   * (e.g., in Python/PHP), but you should prefer variable_definition if
   * you can.
   *
   * less: it would be better to have Assign as a stmt, but most languages
   * allow this at expr level :(
   * Note that IL.ml improves the situation by normalizing this AST with
   * separate expr/instr/stmt types.
   * update: should even be in a separate simple_stmt, as in Go
   *)
  | Assign of
      expr * tok (* '=', '<-' in OCaml. ':=' Go is AssignOp (Eq) *) * expr
  (* less: could desugar in Assign, should be only binary_operator *)
  | AssignOp of expr * operator wrap * expr
  (* newvar:! newscope:? in OCaml yes but we miss the 'in' part here  *)
  | LetPattern of pattern * expr (*e: [[AST_generic.expr]] other assign cases *)
  (* can be used for Record, Class, or Module access depending on expr.
   * In the last case it should be rewritten as a (N IdQualified) with a
   * qualifier though.
   * We choose for now not to have a `dot_operator` type with all the different
     kinds of dots, but instead to translate the `expr` wherever possible.
     So something like `x?.y` is generally translated to an Elvis call on the `x`.
     This will simplify things so that we don't need a brand new type.
   *)
  | DotAccess of expr * tok (* ., ::, ->, #, $ *) * field_name
  (* in Js ArrayAccess is also abused to perform DotAccess (..., FDynamic) *)
  | ArrayAccess of expr * expr bracket
  (* could also use ArrayAccess with a Tuple rhs, or use a special *)
  | SliceAccess of
      expr
      * (expr option (* lower *) * expr option (* upper *) * expr option)
        (* step *)
        bracket
  (* very special value. 'fbody' is usually an FExpr. *)
  | Lambda of function_definition
  (* also a special value; usually an argument of a New
   * (used in Java/Javascript/Scala/...)
   * TODO: rename as NewAnonClass and add the token for 'new'
   *)
  | AnonClass of class_definition
  (* a.k.a ternary expression. Note that even in languages like OCaml
   * where 'if's are expressions, we still prefer to use the stmt 'If'
   * because it allows an optional else part. We need to sometimes
   * wrap those stmts inside an OE_StmtExpr though.
   * TODO: add toks? TODO? in C++ the second expr can be an option
   *)
  | Conditional of expr * expr * expr
  | Yield of tok * expr option * bool (* 'from' for Python *)
  | Await of tok * expr
  (* Send/Recv of Go are currently in OtherExpr *)
  | Cast of type_ * tok (* ':' or leftmost '(' or 'as' *) * expr
  (* less: should be in statement *)
  | Seq of expr list (* at least 2 elements *)
  (* less: could be in Special, but pretty important so I've lifted them here*)
  | Ref of tok (* &, address of *) * expr
  | DeRef of tok (* '*' in C, '!' or '<-' in OCaml, ^ in Reason *) * expr
  (* For YAML aliases
     TODO a better solution would be to use symbolic propagation
     This is a little tricky because YAML is a highly nested expression
     and anchors can be nested. We can get around this by extracting
     the aliases as VarDefs in the beginning and then using them later.
     Revisit when symbolic propagation is more stable
  *)
  | Alias of string wrap * expr
  (* In some rare cases, we need to keep the parenthesis around an expression
   * otherwise in autofix semgrep could produce incorrect code. For example,
   * in Go a cast int(3.0) requires the parenthesis.
   * alt: change cast to take a expr bracket, but this is used only for Go.
   * Note that this data structure is really becoming more a CST than an AST.
   *)
  | ParenExpr of expr bracket
  (* sgrep: ... in expressions, args, stmts, items, and fields
   * (and unfortunately also in types in Python) *)
  | Ellipsis of tok (* '...' *)
  | DeepEllipsis of expr bracket (* <... ...> *)
  | DisjExpr of expr * expr
  | TypedMetavar of ident * tok (* : *) * type_
  (* for ellipsis in method chaining.
   * alt: make it part of field_name in DotAccess
   *)
  | DotAccessEllipsis of expr * tok (* '...' *)
  (* Dual of ExprStmt. See stmt_to_expr() below and its comment.
   * OCaml/Ruby/Scala/... have just expressions, not separate statements.
   *)
  | StmtExpr of stmt
  (* e.g., TypeId in C++, MethodRef/ClassLiteral in Java, Send/Receive in Go,
   * Checked/Unchecked in C#, Repr in Python, RecordWith in OCaml/C#/Jsonnet,
   * Subshell in Ruby, Delete/Unset in JS/Hack/Solidity/C++,
   * Unpack/ArrayAppend in PHP (the AST for $x[] = 1 used to be
   * handled as an AssignOp with special Append).
   * Define/Arguments/NewTarget/YieldStar/Exports/Module/Require/UseStrict JS,
   * UnitLiteral/HexString/UnicodeString/TupleHole/StructExpr in Solidity,
   * AtomExpr/AnonDotField/ContainerBits/When/Join/OpSlashInt/Sigil/Shortcut
   * AttrExpr in Elixir, Error/ImportStr/ObjComprehension in Jsonnet
   * TODO? lift up to program attribute/directive UseStrict, Require in Import?
   * TODO? replace 'any list' by 'expr list'? any way there's still
   * StmtExpr above to wrap stmt if it's not an expr but a stmt
   *)
  | OtherExpr of todo_kind * any list
  (* experimental alternative to OtherExpr. This allows us to have
     proper exprs, stmts, etc. embedded in constructs that were not
     fully translated into the generic AST. *)
  | RawExpr of raw_tree

and literal =
  | Bool of bool wrap
  (* the numbers are an option because OCaml numbers
   * may not be able to represent all numbers. For example, OCaml integers
   * are limited to 63 bits, but C integers can use 64 bits.
   *)
  | Int of int option wrap
  | Float of float option wrap
  | Char of string wrap
  (* String literals:
     The token includes the quotes (if any) but the string value excludes them.
     The value is the escaped string content. For example,
     The escaped content of the Python string literal '\\(\\)' is \\(\\).
     The unescaped content would be \(\).
     TODO: expose the unescaped contents if known, so that we could analyze
     string contents correctly. An incremental change could be:
     | String of (string * string option) bracket
                  ^^^^^^   ^^^^^^
                  escaped  unescaped

     Note that the tokens in the bracket are currently fake tokens in
     many languages. For empty strings, the token in the string wrap is
     also usually a (safe) fake token if the tokens in the brackets are not.
     (see the code in string_literal() far below).
  *)
  | String of string wrap bracket (* can be ', ", or even """ sometimes *)
  (* Regexp literals only. Some languages such as Ruby support regexp
     templates. Those are represented separately using RegexpTemplate. *)
  | Regexp of string wrap bracket (* // *) * string wrap option (* modifiers *)
  | Atom of tok (* ':' in Ruby, ''' in Scala *) * string wrap
  | Unit of tok
  (* a.k.a Void *)
  | Null of tok
  | Undefined of tok (* JS *)
  | Imag of string wrap
  (* Go, Python *)
  | Ratio of string wrap

(* The type of an unknown constant. *)
and const_type = Cbool | Cint | Cstr | Cany

(* semantic value: set by the svalue propagation algorithm and used in semgrep
 *
 * Note that we can't track a constant and a symbolic expression at the same
 * time. If this becomes a problem then we may want to have separate analyses
 * for constant and symbolic propagation, but having a single one is more
 * efficient (time- and memory-wise). *)
and svalue =
  | Lit of literal
  | Cst of const_type
  | Sym of expr
  (* !CAREFUL with Sym!
   * Consider that the smbolic value may itself contain variables that also have
   * symbolic values, and so on. While we (should) prevent cycles, the AST "expanded"
   * with these symbolic values may be exponentially larger than the base AST. This
   * can happen with crypto code. Due to memory sharing this is not a problem for the
   * AST representation itself, but you must be careful when e.g. iterating over ASTs
   * using ksvalue (see Visitor_AST); or e.g. when constructing the Meta_AST. *)
  | NotCst

and container_operator =
  | Array (* todo? designator? use ArrayAccess for designator? *)
  | List
  | Set
  (* a.k.a Hash or Map (combine with Tuple to get Key/value pair) *)
  (* TODO? merge with Record *)
  | Dict
  (* Tuples usually contain at least 2 elements, except for Python where
   * you can actually have 1-uple, e.g., '(1,)'.
   *)
  | Tuple

(* For Python/HCL (and Haskell later). The 'expr' can be a 'Tuple' to
 * represent a Key/Value pair (like in Container). See keyval() below.
 * newscope: for_or_if_comp introduce new local variables whose scope
 *  is just the first expr.
 *)
and comprehension = expr * for_or_if_comp list

(* at least one element *)
and for_or_if_comp =
  (* newvar: *)
  | CompFor of tok (*'for'*) * pattern * tok (* 'in' *) * expr
  | CompIf of tok (*'if'*) * expr

and field_name =
  (* In the case of a field, it may be hard to resolve the id_info inside name.
   * For example, a method id can refer to many method definitions.
   * But for certain things, like a private field, we can resolve it
   * (right now we use an EnclosedVar for those fields).
   *
   * The IdQualified inside name is Useful for OCaml field access.
   *)
  | FN of name
  (* less: FEllipsis instead of DotAccessEllipsis; can also be
   * represented in theory by FDynamic (Ellipsis)
   *)
  (* for PHP/JS fields (even though JS use ArrayAccess for that), or Ruby
   * or C++ ArrowStarAccess ->*
   *)
  | FDynamic of expr

(* It's useful to keep track in the AST of all those special identifiers.
 * They need to be handled in a special way by certain analysis and just
 * using Name for them would be error-prone.
 * Note though that by putting all of them together in a type, we lose
 * typing information. For example, Eval takes only one argument and
 * InstanceOf takes a type and an expr. This is a tradeoff to also not
 * polluate too much expr with too many constructs.
 * TODO: split in IdSpecial of special_id and CallSpecial of special_op
 * and then also just CallOp. And also a separate InterpolatedString.
 *)
and special =
  (* special vars *)
  | This
  | Super (* called 'base' in C# *)
  (* less: how different self/parent is from this/super? *)
  | Self
  | Parent
  (* for Lua, todo: just remove it, create Dict without key *)
  | NextArrayIndex
  (* special calls *)
  | Eval
  | Typeof (* for C? and Go in switch x.(type) *)
  (* todo? make a SpecialType, also use for ClassLiteral *)
  | Instanceof
  | Sizeof (* takes a ArgType *)
  | Defined (* defined? in Ruby, other? *)
  (* used for interpolated strings constructs
   * TODO: move out of 'special' and make special construct InterpolatedConcat
   * in 'expr' instead of abusing Call for that? that way can also
   * avoid those InterpolatedElement stuff.
   *)
  | ConcatString of concat_string_kind
  | EncodedString of string (* only for Python for now (e.g., b"foo") *)
  (* TaggedString? for Javascript, for styled.div`bla{xx}`?
   * We could have this TaggedString where the first arg of Call
   * will be the tagging function, and the rest will be a Call ConcatString.
   * However, it is simpler to just transform those special calls as
   * regular calls even though they do not have parenthesis
   * (not all calls have parenthesis anyway, as in OCaml or Ruby).
   *)
  (* Use this to separate interpolated elements in interpolated strings
   * but this is a bit of a hack.
   * TODO: We should probably add InterpolatedConcat as an expression
   *)
  | InterpolatedElement
  (* "Inline" the content of a var containing a list (a.k.a Splat in Ruby).
   * Used in a Container or Call argument context.
   * The corresponding constructor in a parameter context is ParamRest.
   *)
  | Spread (* ...x in JS, *x in Python/Ruby *)
  (* Similar to Spread, but for a var containing a hashtbl.
   * The corresponding constructor in a parameter context is ParamHashSplat.
   *)
  | HashSplat
    (* **x in Python/Ruby
     * (not to confused with Pow below which is a Binary op *)
  | ForOf (* Javascript, for generators, used in ForEach *)
  (* used for unary and binary operations
   * TODO: move out of special too, in separate OpCall? (where can also
   * have 1 or 2 argument (or maybe even 0 for op reference?)
   *)
  | Op of operator
  (* less: should be lift up and transformed in Assign at stmt level *)
  | IncrDecr of (incr_decr * prefix_postfix)
  (* JS: `require('foo')`. Calls to require are different than imports as
   * represented by e.g. `ImportFrom`. They are expressions rather than top
   * level statements, and can therefore appear inline in any expression, so
   * it's not generally possible to desugar to imports. *)
  | Require

(* mostly binary operators.
 * less: could be divided in really Arith vs Logical (bool) operators,
 * but see is_boolean_operator() helper below.
 * Note that Mod can be used for %style string formatting in Python.
 * Note that Plus can also be used for string concatenations in Go/??.
 * todo? use a Special operator intead for that? but need type info?
 *)
and operator =
  (* unary too *)
  | Plus
  (* unary too *)
  | Minus
  | Mult
  | Div
  | Mod
  | Pow (* ** binary op; for unary see HashSplat above *)
  | FloorDiv
  | MatMult (* Python *)
  | LSL
  | LSR
  | ASR (* L = logic, A = Arithmetic, SL = shift left *)
  | BitOr
  | BitXor
  | BitAnd
  (* unary too *)
  | BitNot
  | BitClear (* Go *)
  (* And/Or are also shortcut operator.
   * todo? rewrite in CondExpr? They have a special behavior.
   *)
  | And
  | Or
  (* PHP has a xor shortcut operator ... hmmm *)
  | Xor
  (* unary *)
  | Not
  | Eq (* '=' in OCaml, '==' in Go/... *)
  | NotEq (* less: could be desugared to Not Eq *)
  | PhysEq (* '==' in OCaml, '===' in JS/... *)
  | NotPhysEq (* less: could be desugared to Not PhysEq *)
  | Lt
  | LtE
  | Gt
  | GtE (* less: could be desugared to Or (Eq Lt) *)
  | Cmp (* <=>, PHP *)
  | Concat (* '.' PHP, '..' Lua *)
  | Append (* x[] = ... in PHP, just in AssignOp *)
  | RegexpMatch (* =~, Ruby (and Perl) *)
  | NotMatch (* !~ Ruby less: could be desugared to Not RegexpMatch *)
  | Range (* .. or ..., Ruby, one arg can be nil for endless range *)
  | RangeInclusive (* '..=' in Rust *)
  | NotNullPostfix (* ! in Typescript, postfix operator *)
  | Length (* '#' in Lua *)
  (* See https://en.wikipedia.org/wiki/Elvis_operator.
   * In PHP we currently generate a Conditional instead of a Binary Elvis.
   * It looks like the Nullish operator is quite similar to the Elvis
   * operator, so we may want to merge those operators at some point.
   *)
  | Elvis (* ?: in Kotlin, can compare possible null value *)
  | Nullish (* ?? in Javascript *)
  | In (* in: checks that value belongs to a collection *)
  | NotIn (* !in *)
  (* Is (and NotIs) checks whether a value has a certain type.
   * The second argument in OpCall is always an ArgType!
   * Can also be used as an unary operation in Kotlin in a 'when'
   *)
  | Is
  | NotIs
  (* Shell & and | *)
  | Background
  | Pipe

(* '++', '--' *)
and incr_decr = Incr | Decr
and prefix_postfix = Prefix | Postfix

and concat_string_kind =
  (* many languages do not require a special syntax to use interpolated
   * strings e.g. simply "this is {a}". Javascript uses backquotes.
   *)
  | InterpolatedConcat (* Javascript/PHP/Ruby/Perl *)
  (* many languages have a binary Concat operator to concatenate strings,
   * but some languages also allow the simple juxtaposition of multiple
   * strings to be concatenated, e.g. "hello" "world" in Python.
   *)
  | SequenceConcat (* Python/C *)
  (* Python requires the special f"" syntax to use interpolated strings,
   * and some semgrep users may want to explicitely match only f-strings,
   * which is why we record this information here.
   * update: also use for interpolated Scala strings
   * update: add string argument to support arbitary string interpolaters in scala
   *)
  | FString of string
  (* Javascript uses a special syntax called tagged template literals, e.g.,
   * foo`template string = ${id}`. We must use a different representation
   * for foo(`template string = ${id}`) because some semgrep users want
   * to find one and not the other.
   * In both case it will be inside Call (Apply (ConcatString  ... but then
   * the kind will differ.
   * example: foo`template ${id}`
   *)
  | TaggedTemplateLiteral

(* This is for JSX/TSX in javascript land (old: and XHP in PHP land).
 * less: we could make it more generic by adding a 'expr so it could be
 * reused in ast_js.ml, ast_php.ml
 *)
and xml = {
  xml_kind : xml_kind;
  xml_attrs : xml_attribute list;
  xml_body : xml_body list;
}

and xml_kind =
  | XmlClassic of tok (*'<'*) * ident * tok (*'>'*) * tok (*'</foo>'*)
  | XmlSingleton of tok (*'<'*) * ident * tok (* '/>', with xml_body = [] *)
  (* React/JS specific *)
  | XmlFragment of tok (* '<>' *) * (* '</>', with xml_attrs = [] *) tok

and xml_attribute =
  | XmlAttr of ident * tok (* = *) * a_xml_attr_value
  (* less: XmlAttrNoValue of ident. <foo a /> <=> <foo a=true /> *)
  (* jsx: usually a Spread operation, e.g., <foo {...bar} /> *)
  | XmlAttrExpr of expr bracket
  (* sgrep: *)
  | XmlEllipsis of tok

(* either a String or a bracketed expr, but right now we just use expr *)
and a_xml_attr_value = expr

and xml_body =
  (* sgrep-ext: can contain "...". The string can also contain multiple lines *)
  | XmlText of string wrap
  (* this can be None when people abuse {} to put comments in it *)
  | XmlExpr of expr option bracket
  | XmlXml of xml

(* brackets can be fake '()' for OCaml/Ruby *)
and arguments = argument list bracket

and argument =
  (* regular argument *)
  | Arg of expr (* can be Call (IdSpecial Spread, Id foo) *)
  (* keyword argument *)
  | ArgKwd of ident * expr
  (* optional keyword argument. This is the same as a keyword argument
     except that a match is valid if such argument exists in the target
     code but not in the pattern.

     Warning: ArgKwdOptional arguments must be placed at the end of the
              list of arguments so as to not shift the positional arguments
              (Arg) and allow them to match.
  *)
  | ArgKwdOptional of ident * expr
  (* type argument for New, instanceof/sizeof/typeof, C macros *)
  | ArgType of type_
  (* e.g., ArgMacro for C/Rust, ArgQuestion for OCaml, ArgIds in Solidity *)
  | OtherArg of todo_kind * any list

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
and stmt = {
  s : stmt_kind;
      [@equal AST_utils.equal_stmt_field_s equal_stmt_kind] [@hash.ignore]
  (* this can be used to compare and hash more efficiently stmts,
   * or in semgrep to quickly know if a stmt is a children of another stmt.
   *)
  s_id : AST_utils.Node_ID.t;
      [@equal AST_utils.equal_stmt_field_s_id] [@name "node_id_t"]
  (* todo? we could store a range: (tok * tok) to delimit the range of a stmt
   * which would allow us to remove some of the extra 'tok' in stmt_kind.
   * Indeed, the main use of those 'tok' is to accurately report a match range
   * in semgrep.
   *)
  mutable s_use_cache : bool; [@equal fun _a _b -> true] [@hash.ignore]
  (* whether this is a strategic point for match result caching.
     This field is relevant for patterns only.

     This applies to the caching optimization, in which the results of
     matching lists of statements can be cached. A list of statements
     is identified by its leading node. In the current implementation,
     the fields 's_id', 's_use_caching', and 's_backrefs' are treated as
     properties of a (non-empty) list of statements, rather than of individual
     statements. A cleaner implementation would consist of a custom
     list type in which each list has these properties, including the
     empty list.
  *)
  mutable s_backrefs : (AST_utils.String_set.t[@name "string_set_t"]) option;
      [@equal fun _a _b -> true] [@hash.ignore]
  (* set of metavariables referenced in the "rest of the pattern", as
     determined by matching order.
     This field is relevant for patterns only.

     This is used to determine which of the bound
     metavariables should be added to the cache key for this node.
     This field is set on pattern ASTs only, in a pass right after parsing
     and before matching.
  *)
  (* used in semgrep to skip some AST matching *)
  mutable s_strings : string Set_.t option;
      [@equal fun _a _b -> true] [@hash.ignore] [@opaque]
  (* used to quickly get the range of a statement *)
  mutable s_range : (Tok.location * Tok.location) option;
      [@equal fun _a _b -> true] [@hash.ignore]
}

and stmt_kind =
  (* See also IL.ml where Call/Assign/Seq are not in expr and where there are
   * separate expr, instr, and stmt types *)
  | ExprStmt of expr * sc (* fake tok in Python, but also in JS/Go with ASI *)
  (* newscope: in C++/Java/Go *)
  | Block of stmt list bracket (* can be fake {} in Python where use layout *)
  (* EmptyStmt = Block [], or separate so can not be matched by $S? $
   * see also emptystmt() at the end of this file.
   *)
  (* newscope: for vardef in condition in C++/Go/... *)
  | If of tok (* 'if' or 'elif' *) * condition * stmt * stmt option
  | While of tok * condition * stmt
  | Return of tok * expr option * sc
  | DoWhile of tok * stmt * expr
  (* newscope: *)
  | For of tok (* 'for', 'foreach'*) * for_header * stmt
  (* The expr can be None for Go and Ruby.
   * less: could be merged with ExprStmt (MatchPattern ...)
   * Also used for 'match' in OCaml/Scala/Rust
   * The 'match'/'switch' token is infix in Scala and C# *)
  | Switch of
      tok
      (* 'switch', also 'select' in Go, or 'case' in Bash, or 'match' in OCaml/Scala/Rust *)
      * condition option
      * case_and_body list (* TODO brace *)
  | Continue of tok * label_ident * sc
  | Break of tok * label_ident * sc
  (* todo? remove stmt argument? more symetric to Goto *)
  | Label of label * stmt
  | Goto of tok * label * sc (* less: use label_ident for computed goto in C*)
  (* TODO? move in expr! in C++ the expr can be an option *)
  | Throw of tok (* 'raise' in OCaml, 'throw' in Java/PHP *) * expr * sc
  | Try of tok * stmt * catch list * finally option
  | WithUsingResource of
      tok (* 'with' in Python, 'using' in C# *)
      * stmt list (* resource acquisition *)
      * stmt (* newscope: block *)
  (* old: was 'expr * expr option' for Python/Java, but better to generalize.
   * alt: could move in expr and have Assert be an IdSpecial
   *)
  | Assert of tok * arguments * sc
  (* TODO? move this out of stmt and have a stmt_or_def_or_dir in Block?
   * or an item list where item is a stmt_or_def_or_dir (as well as field)
   *)
  | DefStmt of definition
  | DirectiveStmt of directive
  (* sgrep: *)
  | DisjStmt of stmt * stmt
  (* This is important to correctly compute a CFG. The any should not
   * contain any stmt! *)
  | OtherStmtWithStmt of other_stmt_with_stmt_operator * any list * stmt
  (* any here should _not_ contain any statement! otherwise the CFG will be
   * incorrect and some analysis (e.g., liveness) will be incorrect.
   * TODO: other_stmt_operator wrap, so enforce at least one token instead
   * of relying that the any list contains at least one token
   *)
  | OtherStmt of other_stmt_operator * any list

(* TODO: can also introduce var in some languages
 * less: factorize with for_var_or_expr
 *)
and condition =
  | Cond of expr
  (* e.g., CondWithDecl, CondDecl in C++ *)
  | OtherCond of todo_kind * any list

(* newscope: *)
(* less: could merge even more with pattern
 * list = PatDisj and Default = PatUnderscore,
 * so case_and_body of Switch <=> action of MatchPattern
 *)
and case_and_body =
  | CasesAndBody of (case list * stmt)
  (* sgrep: *)
  | CaseEllipsis of (* ... *) tok

(* less: we use expr_to_pattern for many languages to build a Case so
 * maybe we should instead have a CaseExpr and CasePattern?
 *)
and case =
  | Case of tok * pattern
  (* less: could unsugar as Case (PatUnderscore _) *)
  | Default of tok
  (* For Go, expr can contain some Assign bindings.
   * todo? could merge with regular Case? can 'case x := <-chan' be
   * transformed in a pattern?
   *)
  | CaseEqualExpr of tok * expr
  (* e.g., CaseRange for C++ *)
  | OtherCase of todo_kind * any list

(* newvar: newscope: usually a PatVar *)
and catch = tok (* 'catch', 'except' in Python *) * catch_exn * stmt

(* alt: we could reuse parameter, which has a ParamPattern and ParamClassic *)
and catch_exn =
  | CatchPattern of pattern
  (* for Java/C++/PHP/etc.
   * old: PatVar of type_ * (ident * id_info) option
   * and was in pattern as PatVar, but better to move out of pattern.
   * alt: we could abuse pattern and use PatTyped, but ugly.
   *)
  | CatchParam of parameter_classic
  (* e.g., CatchEmpty/CatchParams in Solidity *)
  | OtherCatch of todo_kind * any list

(* ptype should never be None *)

(* newscope: *)
and finally = tok (* 'finally' *) * stmt
and label = ident

and label_ident =
  | LNone (* C/Python *)
  | LId of label (* Java/Go/Kotlin *)
  | LInt of int wrap (* PHP *)
  (* PHP, woohoo, dynamic break! bailout for CFG, also in C gccext: goto *)
  | LDynamic of expr

(* todo? put also user-defined iterators here? like MacroIteration in C++ *)
and for_header =
  (* todo? copy Go and have 'of simple option * expr * simple option'? *)
  | ForClassic of
      for_var_or_expr list (* init *)
      * expr option (* cond *)
      * expr option (* next *)
  (* newvar: *)
  | ForEach of for_each
  (* Scala *)
  | MultiForEach of multi_for_each list
  (* Lua. todo: merge with ForEach? *)
  (* pattern 'in' expr *)
  | ForIn of for_var_or_expr list (* init *) * expr list
  (* sgrep: *)
  | ForEllipsis of (* ... *) tok

and for_each =
  pattern
  * tok (* 'in' Python, 'range' Go, 'as' PHP, '' Java, '<-' Scala *)
  * expr (* pattern 'in' expr *)

and multi_for_each =
  | FE of for_each
  | FECond of for_each * tok * expr
  | FEllipsis of tok

and for_var_or_expr =
  (* newvar: *)
  | ForInitVar of entity * variable_definition
  (* less: should rename ForInitAssign really *)
  | ForInitExpr of expr

and other_stmt_with_stmt_operator =
  (* Python/Javascript *)
  (* TODO: used in C# with 'Using', make new stmt TryWithResource? do Java?*)
  | OSWS_With (* newscope: newvar: in OtherStmtWithStmt with LetPattern *)
  (* BEGIN/END in Ruby, Unsafe/Async/Const/Foreign/Impl in Rust,
   * Checked/Unchecked/Lock in C#, Synchronized/Static in Java,
   * Assembly/Unchecked in Solidity
   * alt: use a keyword_attribute instead of todo_kind
   *)
  | OSWS_Block of todo_kind
  (* Ruby *)
  | OSWS_Else_in_try
  (* C/C++/cpp *)
  | OSWS_Iterator
  (* Closures in Swift *)
  | OSWS_Closure
  (* e.g., Case/Default outside of switch in C/C++, StmtTodo in C++ *)
  | OSWS_Todo

and other_stmt_operator =
  (* Python *)
  | OS_Delete
  (* todo: reduce? transpile? *)
  | OS_ForOrElse
  | OS_WhileOrElse
  | OS_TryOrElse
  | OS_ThrowFrom
  | OS_ThrowNothing
  | OS_ThrowArgsLocation
    (* Python2: `raise expr, expr` and `raise expr, expr, exr` *)
  | OS_Pass
  | OS_Async
  (* C/C++ *)
  | OS_Asm
  (* Go *)
  | OS_Go
  | OS_Defer
  | OS_Fallthrough (* only in Switch *)
  (* PHP *)
  | OS_GlobalComplex (* e.g., global $$x, argh *)
  (* Ruby *)
  | OS_Redo
  | OS_Retry
  (* OCaml *)
  | OS_ExprStmt2
  (* Other: Leave/Emit in Solidity *)
  | OS_Todo

(*****************************************************************************)
(* Pattern *)
(*****************************************************************************)
(* This is quite similar to expr. A few constructs in expr have
 * equivalent here prefixed with Pat (e.g., PaLiteral, PatId). We could
 * maybe factorize with expr, and this may help semgrep, but I think it's
 * cleaner to have a separate type because the scoping rules for a pattern and
 * an expr are quite different and not any expr is allowed here.
 *)
and pattern =
  | PatLiteral of literal
  (* Or-Type, used also to match OCaml exceptions.
   * Used with Rust path expressions, with an empty pattern list.
   *)
  | PatConstructor of name * pattern list (* TODO: bracket also here *)
  (* And-Type*)
  | PatRecord of (dotted_ident * pattern) list bracket
  (* newvar:! *)
  | PatId of ident * id_info (* Usually Local/Param, Global in toplevel let *)
  (* special cases of PatConstructor *)
  | PatTuple of pattern list bracket (* at least 2 elements *)
  (* less: generalize to other container_operator? *)
  | PatList of pattern list bracket
  | PatKeyVal of pattern * pattern (* a kind of PatTuple *)
  (* special case of PatId, TODO: name PatAny *)
  | PatUnderscore of tok
  (* OCaml and Scala *)
  | PatDisj of pattern * pattern (* also abused for catch in Java *)
  | PatTyped of pattern * type_
  | PatWhen of pattern * expr (* TODO: add tok, 'when' OCaml, 'if' Scala *)
  | PatAs of pattern * (ident * id_info)
  (* For Go also in switch x.(type) { case int: ... } *)
  | PatType of type_
  (* sgrep: *)
  | PatEllipsis of tok
  | DisjPat of pattern * pattern
  (* e.g., ???
   * todo: Python should transform expr pattern via expr_to_pattern(),
   * so maybe have a PatExpr like we have StmtExpr?
   *)
  | OtherPat of todo_kind * any list

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
and type_ = {
  t : type_kind;
  (* used for C++ and Kotlin type qualifiers *)
  t_attrs : attribute list;
}

and type_kind =
  (* old: was originally TyApply (name, []), but better to differentiate.
   * old: there used to be also a 'TyBuiltin of string wrap' but simpler to
   * just use (TyN Id) instead.
   * todo? a type_builtin = TInt | TBool | ... that mimics the literal type?
   * todo? may need also TySpecial because the name can actually be
   *  self/parent/static (e.g., in PHP)
   *)
  | TyN of name
  (* covers list, hashtbl, etc.
   * note: the type_ should always be a TyN, so really it's a TyNameApply
   * but it's simpler to not repeat TyN to factorize code in semgrep regarding
   * aliasing.
   * TODO: merge with TyN now that qualified_info has type_arguments?
   *)
  | TyApply of type_ * type_arguments
  (* old: was 'TyFun of type_ list * type*' , but languages such as C and
   * Go allow also to name those parameters, and Go even allow ParamRest
   * parameters so we need at least 'type_ * attributes', at which point
   * it's simpler to just reuse parameter.
   *)
  | TyFun of parameter list (* TODO bracket *) * type_ (* return type *)
  (* a special case of TApply, also a special case of TPointer *)
  | TyArray of (* const_expr *) expr option bracket * type_
  | TyTuple of type_ list bracket
  | TyVar of ident (* type variable in polymorphic types (not a typedef) *)
  (* dynamic/unknown type: '_' in OCaml, 'dynamic' in Kotlin, 'auto' in C++,
   * 'var' in Java. TODO: type bounds Scala? *)
  | TyAny of tok
  | TyPointer of tok * type_ (* C/C++/Go *)
  | TyRef of tok * type_ (* C++/Rust *)
  | TyQuestion of type_ * tok (* a.k.a option type *)
  | TyRest of tok * type_ (* '...foo' e.g. in a typescript tuple type *)
  (* intersection types, used for Java Cast, and in Typescript and in
   * Swift for protocol composition *)
  | TyAnd of type_ * tok (* &, or 'with' in Scala *) * type_
  (* union types in Typescript *)
  | TyOr of type_ * tok (* | *) * type_
  (* Anonymous record type, a.k.a shape in PHP/Hack. See also AndType.
   * Most record types are defined via a TypeDef and are then referenced
   * via a TyName. Here we have flexible record types (a.k.a. rows in OCaml).
   *
   * TODO? just generalize as TClass of name option * class_definition
   * for C++. Those are ugly because it would be cleaner to have
   * definitions only at the toplevel, but C/C++/Go allow those nested
   * class defs. We could lift them up at the top and introduce gensym'ed
   * classnames, but it's maybe better to stay close to the code.
   *
   * The Class(Struct) of class_kind are used for Hack and Go, and Interface
   * only for Go.
   *)
  | TyRecordAnon of
      class_kind wrap (* 'struct/shape', fake in other *) * field list bracket
  (* TODO? a TyParen, like ParenExpr? A few languages may need that
   * for autofix to work correctly.
   *)
  (* sgrep-ext: *)
  | TyEllipsis of tok
  (* For languages such as Python which abuse expr to represent types.
   * At some point AST_generic_helpers.expr_to_type should be good enough
   * to transpile every expr construct, but for now we have this.
   * todo? have a TyLiteral of literal for Scala/JS, or use TyExpr for that?
   *)
  | TyExpr of expr
  (* e.g., Struct/Union/Enum names (convert in unique TyName?), TypeOf/TSized
   * TRefRef in C++, EnumAnon in C++/Hack, TyTodo in OCaml, TyLit in Scala/JS,
   * lots of stuff in C#, Lifetime in Rust, Delegation in Kotlin,
   * Any in Swift
   *)
  | OtherType of todo_kind * any list

(* <> in Java/C#/C++/Kotlin/Rust/..., [] in Scala and Go (for Map) *)
and type_arguments = type_argument list bracket

(* TODO? make a record also? *)
and type_argument =
  | TA of type_
  (* Java use-site variance *)
  | TAWildcard of
      tok (* '?' *) * (bool wrap (* extends|super, true=super *) * type_) option
  (* C++/Rust (Rust restrict expr to literals and ConstBlock) *)
  | TAExpr of expr
  (* e.g., Rust Lifetime 'x, Kotlin use-site variance *)
  | OtherTypeArg of todo_kind * any list

(*****************************************************************************)
(* Attribute *)
(*****************************************************************************)
and attribute =
  (* a.k.a modifiers *)
  | KeywordAttr of keyword_attribute wrap
  (* a.k.a decorators, annotations *)
  | NamedAttr of tok (* '@' *) * name * arguments (* less: option *)
  (* e.g,, per-language specific keywords like 'transient', 'synchronized'
   * todo: Expr used for Python, but should transform in NamedAttr when can *)
  | OtherAttribute of todo_kind * any list

and keyword_attribute =
  (* the classic C modifiers (except Auto) *)
  | Static (* a.k.a Intern in Solidity *)
  | Extern (* less: of string? like extern "C" in C++ or Rust *)
  | Volatile
  (* the classic C++ modifiers for fields/methods *)
  | Public
  | Private
  | Protected
  | Abstract (* a.k.a virtual in C++/Solidity *)
  (* for fields/methods in classes and also for classes themselves *)
  | Final
  | Override
  | Mutable (* 'var' in Scala *)
  | Const (* 'readonly' in Typescript, 'val' in Scala *)
  (* for classes (mostly for JVM languages) *)
  (* Scala 'case class', Java 'record', Kotlin 'data class' *)
  | RecordClass
  (* '@interface' in Java, 'annotation class' in Kotlin *)
  | AnnotationClass
  | EnumClass
  (* for Scala and Java *)
  | SealedClass
  (* for variables (JS) *)
  | Var
  | Let
  (* less: should be part of the type? *)
  | Optional
  (* Typescript '?' *)
  | NotNull (* Typescript '!' *)
  (* for functions and methods *)
  | Recursive
  | MutuallyRecursive
  | Generator (* '*' in JS *)
  | Async
  | Inline
  (* for methods *)
  | Ctor
  | Dtor
  | Getter
  | Setter
  (* Rust *)
  | Unsafe
  | DefaultImpl (* Rust unstable, RFC 1210 *)
  (* Scala and Swift *)
  | Lazy
  (* Swift *)
  | Throws
  | Rethrows

(* By name application in Scala, via => T, in parameter *)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* definition (or just declaration sometimes) *)
and definition = entity * definition_kind

(* old: type_: type_ option; but redundant with the type information in
 * the different definition_kind, as well as in id_info, and does not
 * have any meanings for certain defs (e.g., ClassDef) so not worth
 * factoring.
 * update: this could be useful in OCaml when you explicitely type an
 * entity (as in 'let (f: int -> int) = fun i -> i + 1), but we
 * currently abuse id_info.id_type for that.
 *
 * Note that with the new entity_name type and EPattern, one entity value
 * can actually correspond to the definition of multiple vairables.
 *
 * less: could be renamed entity_def, and name is a kind of entity_use.
 *)
and entity = {
  (* In Ruby you can define a class with a qualified name as in
   * class A::B::C, and even dynamically.
   * In C++ you can define a method with a class qualifier outside a class,
   * hence the use of entity_name below and not just ident.
   *)
  name : entity_name;
  attrs : attribute list;
  tparams : type_parameters;
}

(* old: used to be merged with field_name in a unique name_or_dynamic
 * but we want multiple entities (via EPattern) just here, hence the fork.
 *)
and entity_name =
  | EN of name
  | EDynamic of expr
  (* TODO: replace LetPattern and multivardef hack with that *)
  | EPattern of pattern
  (* e.g., AnonBitfield in C++ *)
  | OtherEntity of todo_kind * any list

and definition_kind =
  (* newvar: can be used also for methods or nested functions.
   * note: can have an empty body when the def is actually a declaration
   * in a header file (called a prototype in C).
   *)
  | FuncDef of function_definition
  (* newvar: can be used also for constants.
   * note: can contain special_multivardef_pattern!! ident in which case vinit
   * is the pattern assignment.
   * TODO: still true? We should use EPattern for those cases no?
   *)
  | VarDef of variable_definition
  (* FieldDefColon can be used only inside a record (in a FieldStmt).
   * This used to be merged with VarDef, but in semgrep we don't want
   * a VarDef to match a field definition for certain languages
   * (e.g., JS, OCaml), and we definitely don't want the
   * vardef_to_assign equivalence to be used on FieldDefColon.
   * TODO? maybe merge back with VarDef but add a field in
   *  variable_definition saying whether it's using a colon syntax?
   * TODO? merge instead JS objects with Containers?
   *
   * Note that we could have used a FieldVar in the field type instead
   * of this FieldDef here, which would be more precise, but
   * this complicates things in semgrep where it's convenient to have
   * a uniform FieldStmt(DefStmt) that covers field and methods
   * (see m_list__m_field in semgrep).
   * Note that FieldDefColon where vinit is a Lambda instead be converted
   * in a FuncDef!
   *)
  | FieldDefColon of (* todo: tok (*':'*) * *) variable_definition
  | ClassDef of class_definition
  (* just inside a ClassDef with EnumClass *)
  | EnumEntryDef of enum_entry_definition
  | TypeDef of type_definition
  | ModuleDef of module_definition
  | MacroDef of macro_definition
  (* in a header file (e.g., .mli in OCaml or 'module sig') *)
  | Signature of type_
  (* Only used inside a function.
   * Needed for languages without local VarDef (e.g., Python/PHP)
   * where the first use is also its declaration. In that case when we
   * want to access a global we need to disambiguate with creating a new
   * local.
   *)
  | UseOuterDecl of tok (* 'global' or 'nonlocal' in Python, 'use' in PHP *)
  (* e.g., MacroDecl and MacroVar in C++, method alias in Ruby,
   * BitField in C/C++, Event/Modifier in Solidity, impl in Rust
   *)
  | OtherDef of todo_kind * any list

(* template/generics/polymorphic-type *)
and type_parameter =
  | TP of type_parameter_classic
  (* sgrep-ext: *)
  | TParamEllipsis of tok
  (* e.g., Lifetime in Rust, complex types in OCaml, HasConstructor in C#,
   * regular Param in C++/Go, AnonTypeParam/TPRest/TPNested in C++
   *)
  | OtherTypeParam of todo_kind * any list

and type_parameter_classic = {
  (* it would be nice to reuse entity here, but then the types would be
   * mutually recursive.
   * note: in Scala the ident can be a wildcard.
   *)
  tp_id : ident;
  tp_attrs : attribute list;
  (* upper type bounds (must-be-a-subtype-of)
   * alt: we could just use 'type_' and TyAnd for intersection types *)
  tp_bounds : type_ list;
  (* for Rust/C++. Similar to parameter_classic, but with type here. *)
  tp_default : type_ option;
  (* declaration-site variance (Kotlin/Hack/Scala) *)
  tp_variance : variance wrap option;
}

(* TODO bracket *)
and type_parameters = type_parameter list

(* less: have also Invariant? *)
and variance =
  (* '+' in Scala/Hack, 'out' in C#/Kotlin *)
  | Covariant
  (* '-' in Scala/Hack, 'in' in C#/Kotlin *)
  | Contravariant

(* ------------------------------------------------------------------------- *)
(* Function (or method) definition *)
(* ------------------------------------------------------------------------- *)
(* We could merge this type with variable_definition, and use a
 * Lambda for vinit, but it feels better to use a separate type.
 * TODO? add ctor initializer here instead of storing them in fbody?
 *)
and function_definition = {
  fkind : function_kind wrap;
  fparams : parameters;
  (* return type *)
  frettype : type_ option;
  (* TODO: fthrow *)
  (* newscope: *)
  fbody : function_body;
}

(* We don't really care about the function_kind in semgrep, but who
 * knows, maybe one day we will. We care about the token in the
 * function_kind wrap in fkind though for semgrep for accurate range.
 *)
and function_kind =
  | Function
  (* This is a bit redundant with having the func in a field *)
  | Method
  (* Also redundant; can just check if the fdef is in a Lambda *)
  | LambdaKind
  (* a.k.a short lambdas *)
  | Arrow
  (* for Scala *)
  | BlockCases

(* The brackets are usually '()', but it can be || in Rust (like in Smalltalk)
 * or even sometimes [xxx] in C#.
 *)
and parameters = parameter list bracket

(* newvar: *)
and parameter =
  (* sgrep-ext: note that pname can be a metavariable *)
  | Param of parameter_classic
  (* in OCaml, but also now JS, Python2, Rust *)
  | ParamPattern of pattern
  (* Both those ParamXxx used to be handled as a ParamClassic with special
   * VariadicXxx attribute in p_attr, but they are used in so many
   * languages that it's better to move then in a separate type.
   * We could do a ParamXxx of tok * ident, but some of those params
   * may have attribute, they need a id_info, so simpler to reuse
   * parameter_classic, but pname is always a Some (except for Ruby).
   * ParamRest could be called ParamSpread.
   * alt: move that in ParamPattern instead.
   *)
  | ParamRest of tok (* '...' in JS, '*' in Python *) * parameter_classic
  | ParamHashSplat of tok (* '**' in Python *) * parameter_classic
  (* sgrep: ... in parameters
   * note: foo(...x) of Js/Go is using the ParamRest, not this *)
  | ParamEllipsis of tok
  (* Receiver param in Go, e.g. `func (x Foo) f() { ... }`. This is important
   * for name resolution because Go resolves methods based on the receiver type.
   * *)
  | ParamReceiver of parameter_classic
  (* e.g., ParamTodo in OCaml, SingleStar and Slash
   * in Python to delimit regular parameters from special one.
   * TODO ParamRef of tok * parameter_classic in PHP/Ruby *)
  | OtherParam of todo_kind * any list

(* less: could be merged with variable_definition, or pattern
 * less: could factorize pname/pattrs/pinfo with entity
 *)
and parameter_classic = {
  (* alt: use a 'ParamNoIdent of type_' when pname is None instead? *)
  pname : ident option;
  ptype : type_ option;
  pdefault : expr option;
  pattrs : attribute list;
  (* naming *)
  pinfo : id_info; (* Always Param *)
}

(* old: this used to be just an alias for 'stmt'; we were using
 * fake empty Block for FBDecl of fake ExprStmt for FBExpr.
 * However, some semgreo users may not like to treat a FBStmt
 * pattern to match an FBExpr, hence the more explicit cases.
 *)
and function_body =
  (* usually just a Block (where the brackets are fake in Ruby/Python/...) *)
  | FBStmt of stmt
  (* used for short lambdas in JS/Python, or regular func in OCaml/... *)
  | FBExpr of expr
  (* C/C++ prototypes or interface method declarations in Go/Java/... *)
  | FBDecl of sc
  (* Partial *)
  | FBNothing

(* ------------------------------------------------------------------------- *)
(* Variable definition *)
(* ------------------------------------------------------------------------- *)
(* Also used for constant_definition with attrs = [Const].
 * Also used for field definition in a class (and record).
 * We could use it for function_definition with vinit = Some (Lambda (...))
 * but maybe useful to explicitely makes the difference for now.
 *)
and variable_definition = {
  (* todo? should remove vinit and transform a VarDef with init with a VarDef
   * followed by an Assign (possibly to Null). See vardef_to_assign().
   *)
  vinit : expr option;
  (* less: (tok * expr) option? *)
  vtype : type_ option;
}

(* ------------------------------------------------------------------------- *)
(* Type definition *)
(* ------------------------------------------------------------------------- *)
and type_definition = { tbody : type_definition_kind }

and type_definition_kind =
  (* Algrebraic data types (ADTs), and basic enums.
   * For enum class see class_definition *)
  | OrType of or_type_element list
  (* Record definitions (for struct/class, see class_definition).
   * The fields will be defined via a DefStmt (VarDef variable_definition)
   * where the field.vtype should be defined.
   *)
  | AndType of field list bracket
  (* a.k.a typedef in C (and alias type in Go) *)
  | AliasType of type_
  (* Haskell/Hack/Go ('type x foo' vs 'type x = foo' in Go) *)
  | NewType of type_
  (* OCaml/Rust *)
  | AbstractType of tok (* usually a fake token *)
  | Exception of ident (* same name than entity *) * type_ list
  (* e.g., TdTodo types in OCaml *)
  | OtherTypeKind of todo_kind * any list

and or_type_element =
  (* OCaml *)
  | OrConstructor of ident * type_ list
  (* C enums (for enum class in Java/Kotlin, see EnumClass and EnumEntryDef *)
  | OrEnum of ident * expr option
  (* C union *)
  | OrUnion of ident * type_
  (* sgrep-ext: *)
  | OrEllipsis of tok

(* ------------------------------------------------------------------------- *)
(* Object/struct/record/class field definition *)
(* ------------------------------------------------------------------------- *)

(* Field definition and use, for classes, objects, and records.
 *
 * note: I don't call it field_definition because it's used both to
 * define the shape of a field (a definition), and when creating
 * an actual field (a value).
 * note: It is tempting to want to 'field' be just an alias for 'stmt',
 * but fields can be matched in any order, so it is probably better
 * to keep them separate.
 * note: not all stmt in FieldStmt are definitions. You can have also
 * a Block like in Kotlin for 'init' stmts.
 * However ideally 'field' should really be just an alias for 'definition'.
 *
 * old: there used to be a FieldVar and FieldMethod similar to
 * VarDef and FuncDef but they are now converted into a FieldStmt(DefStmt).
 * This simplifies semgrep so that a function pattern can match
 * toplevel functions, nested functions, and methods.
 * Note that for FieldVar, we sometimes converts it to a FieldDefColon
 * (which is very similar to a VarDef) because some people don't want a VarDef
 * to match a field definition in certain languages (e.g., Javascript) where
 * the variable declaration and field definition have a different syntax.
 * Note: the FieldStmt(DefStmt(FuncDef(...))) can have empty body
 * for interface methods.
 * old: there was a special FieldSpread before (just for JS and Go) but we
 * now abuse ExprStmt(IdSpecial(Spread)) to represent it.
 *)
and field = F of stmt

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
(* less: could be a special kind of type_definition *)
and class_definition = {
  ckind : class_kind wrap;
  (* 'cextends' contains usually 0 or 1 parent, and type_ should be TyN *)
  cextends : class_parent list;
  (* the class_kind in type_ must be Interface *)
  cimplements : type_ list;
  (* the class_kind in type_ are usually Trait *)
  (* PHP 'uses' *)
  cmixins : type_ list;
  (* for Java/Kotlin/Scala RecordClass (we could transpile them into fields) *)
  cparams : parameters;
  (* newscope:
   * note: this can be an empty fake bracket when used in Partial.
   * TODO? use an option here?
   *)
  cbody : field list bracket;
}

(* invariant: this must remain a simple enum; Map_AST relies on it.
 * for EnumClass/AnnotationClass/etc. see keyword_attribute.
 *)
and class_kind =
  | Class (* or Struct for C/Solidity *)
  | Interface (* abused for Contract in Solidity *)
  | Trait
  (* Kotlin/Scala *)
  | Object

(* A parent can have arguments in Scala/Java/Kotlin (because constructors
 * can be defined in the class header via cparams and then this class
 * header can call its parent constructor using those cparams).
 * alt: keep just 'type_' and add constructor calls in cbody.
 * TODO: also can have visibility modifier in C++ or virtual
 *)
and class_parent = type_ * arguments option

(* ------------------------------------------------------------------------- *)
(* Enum entry  *)
(* ------------------------------------------------------------------------- *)
(* for EnumClass, complex enums-as-classes in Java/Kotlin/Scala? *)
and enum_entry_definition = {
  (* the enum identifier is in the corresponding entity *)
  ee_args : arguments option;
  ee_body : field list bracket option;
}

(* ------------------------------------------------------------------------- *)
(* Module definition  *)
(* ------------------------------------------------------------------------- *)
and module_definition = { mbody : module_definition_kind }

and module_definition_kind =
  (* note that those could be converted also in ImportAs *)
  | ModuleAlias of dotted_ident
  (* newscope: *)
  | ModuleStruct of dotted_ident option * item list
  (* e.g., OCaml functors *)
  | OtherModule of todo_kind * any list

(* ------------------------------------------------------------------------- *)
(* Macro definition *)
(* ------------------------------------------------------------------------- *)
(* Used by cpp in C/C++
 * todo? differentiate MacroVar from MacroDef? some macro can take
 * an empty list of parameters, but they are not MacroVar
 *)
and macro_definition = { macroparams : ident list; macrobody : any list }

(*****************************************************************************)
(* Directives (Module import/export, package) *)
(*****************************************************************************)
and directive = {
  d : directive_kind;
  (* Right now d_attrs is used just for Static import in Java, and for
   * OCaml attributes of directives (e.g., open).
   *)
  d_attrs : attribute list;
}

(* It is tempting to simplify all those ImportXxx in a simpler
 * 'Import of dotted_ident * ...', but module_name is not always a DottedName
 * so it is better to clearly separate what is module_name/namespace from an
 * entity (in this module/namespace) even though some languages such as Python
 * blur the difference.
 *)
and directive_kind =
  (* newvar: *)
  | ImportFrom of
      tok (* 'import'/'from' for Python *)
      * module_name
        (* We used to desugar each imported name in an import into its own
         * separate ImportFrom statement. This caused matching issues such as
         * those documented in #5305 and #6532. Removing this desugaring lets us
         * match, for example, a pattern and a target which import the same
         * things but in a different order. *)
      * (ident * alias option (* as name alias *)) list
  | ImportAs of tok * module_name * alias option (* as name *)
  (* Bad practice! hard to resolve name locally.
   * We use ImportAll for C/C++ #include and C++ 'using namespace'.
   * The last tok is '.' in Go, '*' in Java/Python, '_' in Scala, and a fake
   * token in C++ 'using namespace std;'.
   *)
  | ImportAll of tok * module_name * tok
  (* packages are different from modules in that multiple files can reuse
   * the same package name; they are agglomerated in the same package.
   * The dotted_ident can be empty in C++.
   *)
  | Package of tok * dotted_ident (* a.k.a namespace *)
  (* This is used for languages such as C++/PHP/Scala with scoped namespaces.
   * alt: Package of tok * dotted_ident * item list bracket, but less
   * consistent with other directives, so better to use PackageEnd.
   *)
  | PackageEnd of tok
  | Pragma of ident * any list
  (* e.g., Dynamic include in C, Extern "C" in C++/Rust, Undef in C++/Ruby,
   * Export/Reexport in Javascript, Using in Solidity
   * TODO: Declare, move OE_UseStrict here for JS?
   *)
  | OtherDirective of todo_kind * any list

(* xxx as name *)
and alias = ident * id_info

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
(* item (a.k.a toplevel element, toplevel decl) is now equal to stmt.
 * Indeed, many languages allow nested functions, nested class definitions,
 * and even nested imports, so it is just simpler to merge item with stmt.
 * This simplifies semgrep too.
 * TODO? make it an alias to stmt_or_def_or_dir instead?
 *)
and item = stmt
and program = item list

(*****************************************************************************)
(* Partial *)
(*****************************************************************************)
(* sgrep: this is only used by semgrep *)
and partial =
  (* partial defs. The fbody or cbody in definition will be empty. *)
  | PartialDef of definition
  (* partial stmts *)
  | PartialIf of tok * expr (* todo? bracket? TODO? switch to condition? *)
  | PartialTry of tok * stmt
  | PartialCatch of catch
  | PartialFinally of tok * stmt
  | PartialMatch of tok * expr
  (* partial objects (just used in JSON and YAML patterns for now)
   * alt: todo? could be considered a full thing and use Fld?
   * This matches single fields in field list bracket (in Record or cbody).
   * Note that this actually also matches ArgKwd and certain List
   * because in Go composite literals are translated in mix of Calls with
   * ArgKwd and List/Tuples with pairs for fields.
   * alt: have a PartialSingleFieldOrArgKwd special construct for Go?
   *)
  | PartialSingleField of string wrap (* id or str *) * tok (*:*) * expr
  (* not really a partial, but the partial machinery can help with that *)
  | PartialLambdaOrFuncDef of function_definition
  | PartialSwitchCase of case_and_body

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

(* mentioned in many OtherXxx so must be part of the mutually recursive type *)
and any =
  (* main patterns used for semgrep *)
  | E of expr
  | S of stmt
  | Ss of stmt list
  (* also used for semgrep *)
  | T of type_
  | P of pattern
  | At of attribute
  | XmlAt of xml_attribute
  | Fld of field
  | Flds of field list
  | Args of argument list
  | Params of parameter list
  | Xmls of xml_body list
  | Partial of partial
  | Name of name
  | Raw of raw_tree
  (* misc *)
  | I of ident
  | Str of string wrap bracket
  | Def of definition
  | Dir of directive
  | Pr of program
  | Tk of tok
  | TodoK of todo_kind
  | Ar of argument
  | Pa of parameter
  | Tp of type_parameter
  | Ta of type_argument
  | Modn of module_name
  | Ce of catch_exn
  | Cs of case
  | ForOrIfComp of for_or_if_comp
  (* todo: get rid of some? *)
  | ModDk of module_definition_kind
  | En of entity
  | Dk of definition_kind
  | Di of dotted_ident
  | Lbli of label_ident
  (* Used only for Rust macro arguments for now *)
  | Anys of any list

and raw_tree = (any Raw_tree.t[@name "raw_tree_t"])
[@@deriving
  show { with_path = false },
    eq,
    hash,
    (* Autogenerated visitors:
     * http://gallium.inria.fr/~fpottier/visitors/manual.pdf
     *
     * The @name annotations on types above are to disambiguate types that would
     * otherwise be assigned a visitor method named `visit_t`.
     *
     * To view the generated source, build, navigate to
     * `_build/default/libs/ast_generic/`, and then run the following command:
     *
     * ocamlc -stop-after parsing -dsource AST_generic.pp.ml
     * *)
    visitors { variety = "iter"; ancestors = [ "iter_parent" ] },
    visitors { variety = "map"; ancestors = [ "map_parent" ] }]

(*****************************************************************************)
(* Error *)
(*****************************************************************************)

(* This can be used in the xxx_to_generic.ml file to signal limitations.
 * This is captured in Main.exn_to_error to pinpoint the error location.
 * alt: reuse Parse_info.Ast_builder_error exn.
 *)
exception Error of string * Tok.t

let error tok msg = raise (Error (msg, tok))

(*****************************************************************************)
(* Fake tokens *)
(*****************************************************************************)

(* Try avoid using them! if you build new constructs, you should try
 * to derive the tokens in those new constructs from existing constructs
 * and use the Parse_info.fake_info variant, not the unsafe_xxx one.
 *)
let fake s = Tok.unsafe_fake_tok s

(* bugfix: I used to put ";" but now Parse_info.str_of_info prints
 * the string of a fake info
 *)
let sc = Tok.unsafe_fake_tok ""

(*****************************************************************************)
(* AST builder helpers *)
(*****************************************************************************)
(* see also AST_generic_helpers.ml *)

(* ------------------------------------------------------------------------- *)
(* Shortcuts *)
(* ------------------------------------------------------------------------- *)

(* statements *)
let s skind =
  {
    s = skind;
    s_id = AST_utils.Node_ID.mk ();
    s_use_cache = false;
    s_backrefs = None;
    s_strings = None;
    s_range = None;
  }

(* expressions *)
let e ekind = { e = ekind; e_id = 0; e_range = None }

(* directives *)
let d dkind = { d = dkind; d_attrs = [] }

(* types *)
let t tkind = { t = tkind; t_attrs = [] }

(* patterns *)
(* less: nothing yet, but at some point we may want to use a record
 * also for patterns *)
let p x = x

(* ------------------------------------------------------------------------- *)
(* Ident and names *)
(* ------------------------------------------------------------------------- *)

(* For Naming_SAST.ml in deep-semgrep.
 * This can be reseted to 0 before parsing each file, or not. It does
 * not matter as the couple (filename, id_info_id) is unique.
 *)
let id_info_id = IdInfoId.mk
let empty_var = { vinit = None; vtype = None }

let empty_id_info ?(hidden = false) () =
  {
    id_resolved = ref None;
    id_type = ref None;
    id_svalue = ref None;
    id_hidden = hidden;
    id_info_id = id_info_id ();
  }

let basic_id_info ?(hidden = false) resolved =
  {
    id_resolved = ref (Some resolved);
    id_type = ref None;
    id_svalue = ref None;
    id_hidden = hidden;
    id_info_id = id_info_id ();
  }

(* TODO: move AST_generic_helpers.name_of_id and ids here *)

let dotted_to_canonical xs = Common.map fst xs
let canonical_to_dotted tid xs = xs |> Common.map (fun s -> (s, tid))

(* ------------------------------------------------------------------------- *)
(* Entities *)
(* ------------------------------------------------------------------------- *)

(* alt: could use @@deriving make *)
let basic_entity ?hidden ?(attrs = []) ?(tparams = []) id =
  let idinfo = empty_id_info ?hidden () in
  { name = EN (Id (id, idinfo)); attrs; tparams }

(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)

(* easier to use in List.map than each time (fun e -> Arg e) *)
let arg e = Arg e

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)
let special spec es =
  Call (IdSpecial spec |> e, Tok.unsafe_fake_bracket (es |> Common.map arg))
  |> e

let opcall (op, tok) exprs : expr = special (Op op, tok) exprs

let string_ (lquote, xs, rquote) : string wrap bracket =
  let s = xs |> Common.map fst |> String.concat "" in
  let t =
    match xs with
    | [] -> Tok.fake_tok lquote ""
    | (_, t) :: ys -> Tok.combine_toks t (Common.map snd ys)
  in
  (lquote, (s, t), rquote)

(* TODO: have a separate InterpolatedConcat in expr with a cleaner type
 * instead of abusing special?
 *)
let interpolated (lquote, xs, rquote) =
  match xs with
  | [ Common.Left3 (str, tstr) ] ->
      L (String (lquote, (str, tstr), rquote)) |> e
  | __else__ ->
      let special = IdSpecial (ConcatString InterpolatedConcat, lquote) |> e in
      Call
        ( special,
          ( lquote,
            xs
            |> Common.map (function
                 | Common.Left3 x ->
                     Arg (L (String (Tok.unsafe_fake_bracket x)) |> e)
                 | Common.Right3 (lbrace, eopt, rbrace) ->
                     let special =
                       IdSpecial (InterpolatedElement, lbrace) |> e
                     in
                     let args = eopt |> Option.to_list |> Common.map arg in
                     Arg (Call (special, (lbrace, args, rbrace)) |> e)
                 | Common.Middle3 e -> Arg e),
            rquote ) )
      |> e

(* todo? use a special construct KeyVal valid only inside Dict? *)
let keyval k _tarrow v =
  Container (Tuple, Tok.unsafe_fake_bracket [ k; v ]) |> e

let raw x = RawExpr x |> e

(* ------------------------------------------------------------------------- *)
(* Parameters *)
(* ------------------------------------------------------------------------- *)

(* alt: could use @@deriving make *)
let param_of_id ?(pattrs = []) ?(ptype = None) ?(pdefault = None) id =
  {
    pname = Some id;
    pdefault;
    ptype;
    pattrs;
    pinfo = basic_id_info (Parameter, SId.unsafe_default);
  }

let param_of_type ?(pattrs = []) ?(pdefault = None) ?(pname = None) typ =
  {
    ptype = Some typ;
    pname;
    pdefault;
    pattrs;
    pinfo = basic_id_info (Parameter, SId.unsafe_default);
  }

(* for 'function 0 -> 1 ...' in OCaml or 'do 0 -> 1 ...' in Elixir *)
let implicit_param_id tk = ("!_implicit_param!", tk)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
let ty_builtin id = TyN (Id (id, empty_id_info ())) |> t

(* ------------------------------------------------------------------------- *)
(* Type parameters *)
(* ------------------------------------------------------------------------- *)
let tparam_of_id ?(tp_attrs = []) ?(tp_variance = None) ?(tp_bounds = [])
    ?(tp_default = None) tp_id =
  TP { tp_id; tp_attrs; tp_variance; tp_bounds; tp_default }

(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)

let exprstmt e = s (ExprStmt (e, sc))

(* alt: EmptyStmt of sc? of ExprStmt of expr option * sc *)
let emptystmt t = s (Block (t, [], t))

(* The dual of exprstmt.
 * This is mostly used for languages where the division
 * between stmt and expr is fuzzy or nonexistent (e.g., OCaml, Scala)
 * and where things like While, Match are expressions, but in the
 * generic AST they are statements.
 * See also AST_generic_helpers with expr_to_pattern, expr_to_type,
 * pattern_to_expr, etc.
 *)
let stmt_to_expr st = e (StmtExpr st)
let empty_body = Tok.unsafe_fake_bracket []

let stmt1 xs =
  match xs with
  | [] -> s (Block (Tok.unsafe_fake_bracket []))
  | [ st ] -> st
  | xs -> s (Block (Tok.unsafe_fake_bracket xs))

(* ------------------------------------------------------------------------- *)
(* Fields *)
(* ------------------------------------------------------------------------- *)

(* this should be simpler at some point if we get rid of FieldStmt *)
let fld (ent, def) = F (s (DefStmt (ent, def)))

let basic_field id vopt typeopt =
  let entity = basic_entity id in
  fld (entity, VarDef { vinit = vopt; vtype = typeopt })

let fieldEllipsis t = F (exprstmt (e (Ellipsis t)))

(* ------------------------------------------------------------------------- *)
(* Attributes *)
(* ------------------------------------------------------------------------- *)

let attr kwd tok = KeywordAttr (kwd, tok)

let unhandled_keywordattr (s, t) =
  (* TODO? or use OtherAttribue? *)
  NamedAttr (t, Id ((s, t), empty_id_info ()), Tok.unsafe_fake_bracket [])

(* ------------------------------------------------------------------------- *)
(* Patterns *)
(* ------------------------------------------------------------------------- *)

let case_of_pat_and_expr ?(tok = None) (pat, expr) =
  let tok =
    match tok with
    | None -> fake "case"
    | Some tok -> tok
  in
  CasesAndBody ([ Case (tok, pat) ], exprstmt expr)

let case_of_pat_and_stmt ?(tok = None) (pat, stmt) =
  let tok =
    match tok with
    | None -> fake "case"
    | Some tok -> tok
  in
  CasesAndBody ([ Case (tok, pat) ], stmt)

(*****************************************************************************)
(* Special constants *)
(*****************************************************************************)

(* In JS one can do 'var {x,y} = foo();'. We used to transpile that
 * in multiple vars, but in semgrep one may want to match over those patterns.
 * However those multivars do not fit well with the (entity * definition_kind)
 * model we currently use, so for now we need this ugly hack of converting
 * the statement above in
 * ({name = "!MultiVarDef"}, VarDef {vinit = Assign (Record {...}, foo())}).
 * This is bit ugly, but at some point we may want to remove completely
 * VarDef by transforming them in Assign (see vardef_to_assign() below)
 * so this temporary hack is not too bad.
 * TODO: use EPattern instead
 *)
let special_multivardef_pattern = "!MultiVarDef!"

(*****************************************************************************)
(* Semgrep hacks *)
(*****************************************************************************)

(* !!You should not use the function below!! You should use instead
 * Metavars_generic.is_metavar_name. If you use the function below,
 * it probably means you have an ugly dependency to semgrep that you
 * should not have.
 * coupling: Metavariable.is_metavar_name
 *)
let is_metavar_name s = Common.( =~ ) s "^\\(\\$[A-Z_][A-Z_0-9]*\\)$"

(* coupling: Metavariable.is_metavar_ellipsis *)
let is_metavar_ellipsis s =
  Common.( =~ ) s "^\\(\\$\\.\\.\\.[A-Z_][A-Z_0-9]*\\)$"

(*****************************************************************************)
(* Custom visitors *)
(*****************************************************************************)

(* Most clients should use this instead of the default `iter`. In many cases,
 * it's not desirable to recurse into id_info since it contains resolved names
 * and svalues which often contain nodes that are already present elsewhere in
 * the AST. This matches the default behavior of the old mk_visitor. *)
class virtual ['self] iter_no_id_info =
  object (_self : 'self)
    inherit ['self] iter
    method! visit_id_info _env _info = ()
  end

(* This is based on the legacy behavior of Map_AST.mk_visitor (look through the
 * commit history if you want to take a trip down memory lane). It was a bunch
 * of hardcoded boilerplate, which hid this nonstandard behavior.
 *
 * If you are adding a new callsite, you should consider whether the nonstandard
 * behavior here makes sense for your use case. Think about using the
 * autogenerated AST_generic.map directly. *)
class virtual ['self] map_legacy =
  object (self : 'self)
    inherit [_] map
    method! visit_tok _env v = v

    method! visit_expr env x =
      let ekind = self#visit_expr_kind env x.e in
      (* TODO? reuse the e_id or create a new one? *)
      e ekind

    (* For convenience, so clients don't need to override visit_arguments and
     * deal with the bracket type. *)
    method visit_argument_list env v = self#visit_list self#visit_argument env v

    (* Override to call visit_argument_list *)
    method! visit_arguments env v =
      self#visit_bracket self#visit_argument_list env v

    method! visit_stmt env x =
      let skind = self#visit_stmt_kind env x.s in
      { x with s = skind }
  end
