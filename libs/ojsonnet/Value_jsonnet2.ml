
module A = AST_jsonnet
module C = Core_jsonnet2

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

and object_ = {asserts: obj_assert clo list; fields: field list; super: object_ A.bracket option}

and obj_assert = A.tok (* assert *) * C.expr

and field = {
  fld_name : string A.wrap;
  fld_hidden : C.hidden A.wrap;
  fld_value : C.expr clo;
}

and env = {
  depth: int;
  locals: (string, value Lazy.t) Map_.t;
  self: object_ A.bracket option;
}

and 'a clo = {env: env [@opaque]; body: 'a}

[@@deriving show]

let show_field_names fields =
  fields |> List.map (fun {fld_name = (fld,_); _} -> fld ^ " ") |> String.concat ", "

let bind env name value =
  {env with locals = Map_.add name value env.locals}

let bind_all env bindings =
  List.fold_left (fun env (name, value) ->
    bind env name value) env bindings

let empty_env = {depth = 0; locals = Map_.empty; self = None}

let show_env env = show_env {env with locals = env.locals |> Map_.remove "std" |> Map_.remove "$"}