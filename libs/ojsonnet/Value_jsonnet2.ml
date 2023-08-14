module A = AST_jsonnet
module E = Core_jsonnet

type value =
  | Self of object_ A.bracket ref
  | Primitive of primitive
  | Object of object_ A.bracket
  | Lambda of E.function_definition
  | Array of value Lazy.t array A.bracket

and primitive =
  | Null of A.tok
  | Bool of bool A.wrap
  | Double of float A.wrap
  | Str of string A.wrap

and object_ = {
  asserts : asserts list;
  fields : value_field list;
  super : object_ A.bracket option;
}
(* opti? make it a hashtbl of string -> field for faster lookup? *)

and value_field = {
  (* like Str, strictly evaluated! *)
  fld_name : string A.wrap;
  fld_hidden : A.hidden A.wrap;
  (* Whena field value is overwritten by composition, keep the original around for access via super *)
  fld_values : value Lazy.t list;
}

and asserts = Core_jsonnet.obj_assert
and lazy_value = [ `Forced of value | `Lazy of A.expr ] ref

and env = {
  depth : int;
  locals : (string, lazy_value) Map_.t;
  (* if inside an object *)
  self : object_ A.bracket option;
}
[@@deriving show]

let force eval lzy =
  match !lzy with
  | `Forced v -> v
  | `Lazy e ->
      let v = eval e in
      lzy := `Forced v;
      v

let empty_obj : value =
  let fk = Tok.unsafe_fake_tok "" in
  Object (fk, { asserts = []; fields = []; super = None }, fk)

let empty_env = { locals = Map_.empty; depth = 0; self = None }
