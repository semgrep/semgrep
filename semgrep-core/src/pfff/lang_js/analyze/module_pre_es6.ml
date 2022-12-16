
(* The goal is to gather module-level information from a Javascript file.
 * Given this toy Javascript file:
 *
 * /* @providesModule Foo */
 *
 * var Bar = require('Bar');
 * function Foo(x) { this.x = x; }
 * Foo.prototype = { f: function() { } }
 * copyProperties(Foo.prototype, Bar.prototype)
 * module.exports = Foo;
 *
 * the infered moduleinfo is of the form (shape, local_state)
 *
 * where shape (eliding a few details) =
 *
 * FunctionShape (ClassShape (
 *   ObjectShape({"x" => ...}),
 *	 ObjectShape({
 *	   "prototype" => ObjectShape(
 *	     { "f" => FunctionShape (...) },
 *	     [ PropertyShape(RequireShape("Bar"),"prototype") ]
 *	   )
 *   })
 * ))
 *
 * and local_state =
 * {
 *   module_ = "Foo",
 *	 local_requires = ["Bar"],
 * 	 local_bindings = {
 *	   "Bar" => Require("Bar"),
 *	   "Foo" => shape
 *	 }
 * }
*)

open Common

(****************************)
(* module-level information *)
(****************************)

type module_ = string

type shape =

  | LiteralShape
  | ArrayShape

  (* _((id,container,maps) ref) *)
  (* this is a ref to allow extensible representations *)
  (* id is unique, and is used to prune infinite recursion *)
  (* maps is an ObjectShape list *)
  | ObjectShape of (int * shape smap * shape list) ref

  (* _(block,constructor) *)
  (* block is an ObjectShape *)
  (* constructor is a ClassShape *)
  | FunctionShape of shape Common.smap ref * shape

  (* _(module) *)
  | RequireShape of module_

  (* _(reason) *)
  | UnknownShape of string

  (* _(instance, static) *)
  (* instance is a ObjectShape *)
  (* static is a ObjectShape where static.prototype is a ObjectShape *)
  | ClassShape of shape * shape

  (* _(class) *)
  (* class is a ClassShape *)
  (* returns an ObjectShape *)
  | NewShape of shape

  (* _(maps) *)
  (* maps is an ObjectShape *)
  (* returns a ClassShape *)
  | MixinShape of shape

  (* _(class,mixin) *)
  (* class is a ClassShape, mixin is a ClassShape *)
  (* returns a ClassShape *)
  | ClassWithMixinShape of shape * shape

  (* _(object,prop) *)
  | PropertyShape of shape * string

  (* _(function) *)
  | ApplyShape of shape

  (* _(array) *)
  | ElementShape of shape

let fresh_id =
  let id = ref 0 in
  fun () -> (id := !id + 1; !id)

let new_object(map) = ObjectShape (ref (fresh_id(), map, []))
let new_class() =
  new_object(SMap.singleton "prototype" (new_object SMap.empty))
let rootclass = MixinShape (new_object SMap.empty)

let is_empty_object o = match o with
  | ObjectShape {contents = (_,map,[])} -> SMap.is_empty map
  | _ -> false

let is_empty_class o = match o with
  | ObjectShape {contents = (_,map,[])} ->
      (SMap.cardinal(map) = 1) && (is_empty_object(SMap.find "prototype" map))
  | _ -> false


(****************)
(* print utils 	*)
(****************)

let indent n = String.make (2*n) ' '

let mk_indent n s = (indent n) ^ s ^ "\n"

let mk_id id = "#" ^ (string_of_int id)

let rec string_of_shape_ n stack shape =
  match shape with

  | LiteralShape -> mk_indent n "literal"

  | FunctionShape (_,shape) ->
      (mk_indent n "function") ^
      (string_of_shape_ (n+1) stack shape)

  | ArrayShape -> mk_indent n "array"

  | ObjectShape {contents = (id,map,maps)} ->
      if (List.mem id stack) then (mk_indent n ((mk_id id) ^ "...")) else
        (SMap.fold
           (fun prop -> fun shape -> fun str ->
              str ^
              (mk_indent (n+1) (prop ^ ":")) ^
              (string_of_shape_ (n+2) (id::stack) shape)
           )
           map (mk_indent n ((mk_id id) ^ " {"))
        ) ^
        (List.fold_left
           (fun str -> fun map ->
              str ^
              (mk_indent (n+1) ("|")) ^
              (string_of_shape_ (n+2) (id::stack) map)
           )
           (mk_indent n "}") maps
        )

  | ClassShape (instance, static)
    when (is_empty_object instance) && (is_empty_class static) ->
      ""

  | ClassShape (instance, static) ->
      (mk_indent n "class {") ^
      (mk_indent (n+1) "instance: ") ^
      (string_of_shape_ (n+2) stack instance) ^
      (mk_indent (n+1) "static: ") ^
      (string_of_shape_ (n+2) stack static) ^
      (mk_indent n "}")

  | RequireShape m ->
      mk_indent n ("require " ^ m)

  | PropertyShape (shape,x) ->
      (mk_indent n ("." ^ x)) ^
      (string_of_shape_ (n+1) stack shape)

  | UnknownShape reason ->
      mk_indent n ("unknown: " ^ reason)

  | NewShape _class ->
      (mk_indent n "new") ^
      (string_of_shape_ (n+1) stack _class)

  | MixinShape map ->
      (mk_indent n "mixin") ^
      (string_of_shape_ (n+1) stack map)

  | ClassWithMixinShape (_class, mixin) ->
      (mk_indent n "+") ^
      (string_of_shape_ (n+1) stack _class) ^
      (string_of_shape_ (n+1) stack mixin)

  | ApplyShape _function ->
      (mk_indent n "(...)") ^
      (string_of_shape_ (n+1) stack _function)

  | ElementShape _array ->
      (mk_indent n "[...]") ^
      (string_of_shape_ (n+1) stack _array)

let string_of_shape n shape =
  string_of_shape_ n [] shape

type parseinfo = (Ast_js.a_program * Parser_js.token list)

type parseinfo_map = parseinfo smap

type moduleinfo = {
  module_: module_;
  local_requires: module_ list;
  local_bindings: shape smap
}

type moduleinfo_map = moduleinfo smap
