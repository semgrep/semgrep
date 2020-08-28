(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
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
module Graph = Dependencies_toposort_php.Graph

module ISet = Set.Make(Int)
module IMap = Map.Make(Int)
module SSet = Set.Make(String)
module SMap = Map.Make(String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Main types and data structures used by the PHP type inference engine:
 * The PHP types representation 't' and the 'environment'.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* A tentative PHP type system. It has polymorphic types, union types,
 * and object types. It also has some special support for constants
 * and arrays because they are abused respectively to represent
 * enums and records and we want to infer this information back.
 *)
type t =
  (* polymorphic type variable, 'a, 'b, ... *)
  | Tvar of int
  (* union type *)
  | Tsum of prim_ty list

  and prim_ty =
    (* Any abstract type, e.g. int, bool, string, etc. but also null *)
    | Tabstr of string

    (* A set of static strings *)
    | Tsstring of SSet.t
    (* An enum of integers, class MyEnum { static const XX = 0; } *)
    | Tienum of SSet.t
    (* An enum of strings, class MyEnum { static const XX = 'foo'; } *)
    | Tsenum of SSet.t

    (* arrays are used for everything in PHP, but for the type inference
     * we actually want to differentiate those different usages: record
     * vs real array vs hash.
     *)
    | Trecord of t SMap.t
    (* An array, where we don't know what all the possible field names are.
     * The SSet contains all the field names we have seen so far.
     * Example: $x = array('foo' => 0); $x[] = 12; will translate in
     * Tarray (SSet('foo'), int | string, int)
     *)
    (* use Tarray to represent a "confused" array classification*)
    | Tarray  of SSet.t * t * t

    (* this is also used for methods *)
    | Tfun    of (string * t) list * t

    (* A class is represented as a Tclosed/Tobject with 
     * a special field called __obj that contains the type of the
     * instanciated object.
     *
     * Example:
     *  class A {
     *    public static function f() { }
     *    public function g() { }
     *  }
     *  is represented as
     *  Tclosed (SSet('A'), SMap(
     *    'f': function unit -> unit
     *    '__obj': SMap (
     *      'g' => function unit -> unit
     *    )
     *  )
     *)
    | Tobject of t SMap.t
    (* Same as Tobject, except that we know the set of possible classes that
     * were used to instanciate the object.
     *)
    | Tclosed of SSet.t * t SMap.t

(* todo: reuse Env_interpreter_php.code_database? *)
type code_database = {
    classes: Ast_php.class_def Common2.cached SMap.t ref;
    funcs: Ast_php.func_def Common2.cached SMap.t ref;
    (* todo: constants?? *)
}

(* Module in order to create a map with array identifiers for the key. 
 * Compare function declares equivalence, but no other ordering. 
 *)
module Array_id = 
struct
  type t  = Ast_php.expr * string * string

  let compare a b = 
    match a, b with 
    | (e1, f1, c1), (e2, f2, c2) when f1 = f2 && c1 = c2 && e1 = e2 ->
        0
    | _ -> -1
end 

(* Map with an array id as the key type *)
module AMap = Map.Make(Array_id)

type env = {
    db: code_database;

    (* The graph of dependencies. If foo() calls bar(), then
     * there will be a dependency between 'foo' and 'bar' and
     * we will want to first infer the type of 'bar' before 'foo'
     * (because we do a bottom-up type inference).
     *)
    graph: Graph.t;

    (* less: this is just used to remember the builtins and
     * not print them when we list all the infered types. Could
     * be removed.
     *)
    builtins: SSet.t ref;

    (* The local variables environment *)
    vars: t SMap.t ref;

    (* The global variable environment. This also contains typing
     * information for functions and classes. The "^Class:", "^Function:"
     * and "^Global:" prefixes are (ab)used for "namespace" (ugly, abusing
     * strings again).
     * Builtins constants such as 'null' are stored here as functions.
     *)
    globals: t SMap.t ref;

    (* The array environment. Contains all typing information for all arrays *)
    aenv: arr_info list AMap.t ref;

    (* The current function being typed, for the purpose of array
     * identification *)
    mutable aenv_fun: string;

    (* The current class being typed, for the purpose of array identificatio*)
    mutable aenv_class: string; 

    (* List of parameters for the current function being typed. Note that all
     * parameters need not be arrays, but that this list must be crossed with
     * aenv to determine the arrays which are parameters *)
    aenv_params: Array_id.t list ref;

    aenv_funs: (string * string * Parse_info.t) list ref; (*function, class, file*)
    
    (* The typing environment (pad: mapping type variables to types?) *)
    tenv: t IMap.t ref;
    (* The current substitution (for type variables). This will
     * tell if 'a -> 'b, that is 'a was unified at some point with 'b.
     * This will grow a lot when we process the whole codebase.
     * See typing_unify_php.ml for more information.
     *)
    subst: int IMap.t ref;



    (* Shall we show types with the special marker? *)
    infer_types: bool;
    (* Shall we perform autocompletion? *)
    auto_complete: bool;

    (* Marker used in interactive mode *)
    marker: string;

    verbose: bool;
    (* throw exception instead of passing over undefined constructs *)
    strict: bool;
    (* pad: ?? *)
    depth: int;
    (* The types to show *)
    show: show ref;
    (* Are we in debug mode *)
    debug: bool;

    (* The total amount of classes/functions to type *)
    total: int ref;
    (* The total amount of classes/functions typed so far *)
    count: int ref;
    (* The internal counter for garbage collection *)
    collect_count: int ref;
    (* The cumulated garbage collection time *)
    cumul: float ref;
  }

(* This is used to describe the type of interactions with an array, and the
 * location of this interaction. *)
and arr_info = Parse_info.t option * arr_access

(* Type signifying the kind of interactions with arrays seen in the ast *)
and arr_access = 
  | NoIndex of t (*v*)
  | VarOrInt of t * t (*k, v*)
  | Const of t (*v*)
  | ConstantString of t (*v*)
  | Declaration of t list
  | Value of t
  | DeclarationKValue of t * t
  | DeclarationValue of t
  | UnhandledAccess
  | Parameter
  | ReturnValue


(* This is used for the autocompletion and interactive type inference
 * in Emacs (Tab and C-c C-t).
 * todo: meaning?
 *)
and show =
  | Snone
  | Stype_infer of t
  | Sauto_complete of string * t
  | Sargs of t
  | Sglobal of string
  | Slocal of string * SSet.t

(*****************************************************************************)
(* Projection *)
(*****************************************************************************)

(* In a Tsum we want the different possible types to be sorted so
 * that unifying two Tsum and finding common stuff can be done quickly.
 * Proj is used to give an order between types, and when two things
 * are equivalent (such as a Tobject and Tclosed), we project on
 * the same value.
 *)
let rec proj = function
  | Tabstr ("int" | "bool" | "string" | "html") -> Hashtbl.hash "string"
  | Tabstr x -> Hashtbl.hash x
  | Tsstring _ -> Hashtbl.hash "string"
  | Tienum _ -> proj (Tabstr "int")
  | Tsenum _ -> proj (Tabstr "string")
  (* pad: how do we know those values can not match any of the preceding
   * Hashtbl.hash values?
   *)
  | Trecord _ -> 2
  | Tarray  _ -> 2
  | Tfun    _ -> 3
  | Tobject _ -> 4
  | Tclosed _ -> 4

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let make_env () = {
  db = {
    classes = ref SMap.empty;
    funcs = ref SMap.empty;
  };
  builtins = ref SSet.empty;
  graph = Graph.empty();

  vars     = ref SMap.empty;
  globals    = ref SMap.empty;
  (*Contains typing information regarding all array declarations and accesses*)
  aenv    = ref AMap.empty;
  (*Current function name for the purpose of differentiating arrays in different
   * functions*)
  aenv_fun = "";
  aenv_funs = ref [];
  aenv_class = "";
  aenv_params = ref [];

  tenv    = ref IMap.empty;
  subst   = ref IMap.empty;

  depth   = 0;
  show   = ref Snone;

  debug = false;
  infer_types = false;
  auto_complete = false;
  verbose = true;
  strict = false;
  marker = "JUJUMARKER";

  total = ref 0;
  count = ref 0;
  collect_count = ref 0;
  cumul = ref 0.0;
}

(*****************************************************************************)
(* Shortcuts *)
(*****************************************************************************)

let pbool   = Tabstr "bool"
let pint    = Tabstr "int"
let pfloat  = Tabstr "float"
let pstring = Tabstr "string"
let pnull   = Tabstr "null"
let phtml   = Tabstr "html"

let fresh =
  let i = ref 0 in
  fun () -> incr i; !i

let fvar() = Tvar (fresh())

let bool   = Tsum [pbool]
let int    = Tsum [pint]
let thtml  = Tsum [phtml]
let float  = Tsum [pfloat]
let string = Tsum [pstring]
let null   = Tsum [pnull]

let any = Tsum []

let empty = Tsum [Trecord SMap.empty]
let array(t1, t2) = Tsum [Tarray (SSet.empty, t1, t2)]
let srecord(s, v) = Tsum [Trecord (SMap.add s v SMap.empty)]
let sobject(s, v) = Tsum [Tobject (SMap.add s v SMap.empty)]
let fun_ l b = Tsum [Tfun (List.map (fun x -> "", x) l, b)]
let afun l b = Tsum [Tfun (l, b)]
