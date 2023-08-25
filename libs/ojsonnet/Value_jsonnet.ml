
module A = AST_jsonnet
module C = Core_jsonnet

type value =
  | Primitive of primitive
  | Object of object_ A.bracket
  | Lambda of C.function_definition clo
  | Array of value Lazy.t array A.bracket

and primitive =
  | Null of A.tok
  | Bool of bool A.wrap
  | Double of float A.wrap
  | Str of string A.wrap

and object_ = {layers: layers; field_names: unit field list}

and layers = object_data list

and object_data = {asserts: obj_assert clo list; fields: (C.expr clo) field list}

and obj_assert = A.tok (* assert *) * C.expr

and arg =
  | Arg of value Lazy.t
  | NamedArg of C.ident * A.tok * value Lazy.t

and 'a field = {
  fld_name : string A.wrap;
  fld_hidden : C.hidden A.wrap;
  fld_value : 'a;
}

and env = {
  depth: int;
  locals: (string, bind) Map_.t;
  self: object_ A.bracket option;
  super_level: int;
}

and bind =
  | Rec of (string * C.expr clo) list
  | Nonrec of value Lazy.t

and 'a clo = {env: env [@opaque]; body: 'a}

[@@deriving show]

let show_field_names fields =
  fields |> List.map (fun {fld_name = (fld,_); _} -> fld ^ " ") |> String.concat ", "

let bind env name value =
  {env with locals = Map_.add name value env.locals}

let bind_all env bindings =
  List.fold_left (fun env (name, value) ->
    bind env name value) env bindings

let empty_env = {depth = 0; super_level = 0; locals = Map_.empty; self = None}

let show_env env = show_env {env with locals = env.locals |> Map_.remove "std" |> Map_.remove "$"}

let fields (_,{fields;_},_) = fields
