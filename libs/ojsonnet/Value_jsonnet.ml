
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



and object_ = {asserts: obj_assert clo list; fields: field list list}

and obj_assert = A.tok (* assert *) * C.expr

and arg = 
  | Arg of value Lazy.t
  | NamedArg of C.ident * A.tok * value Lazy.t

and field = {
  fld_name : string A.wrap;
  fld_hidden : C.hidden A.wrap;
  fld_value : C.expr clo;
  fld_super: object_ A.bracket option;
}

and env = {
  depth: int;
  locals: (string, bind) Map_.t;
  self: (object_ A.bracket * string A.wrap option) option;
}

and bind =
  | Rec of (string * C.expr clo) list
  | Nonrec of value Lazy.t

and 'a clo = {env: env [@opaque]; body: 'a}

[@@deriving show]

let show_field_names (_,{fields;_},_) =
  fields |> List.map (fun fs -> match List.hd fs with {fld_name = (fld,_); _} -> fld ^ " ") |> String.concat ", "

let bind env name value =
  {env with locals = Map_.add name value env.locals}

let bind_all env bindings =
  List.fold_left (fun env (name, value) ->
    bind env name value) env bindings

let empty_env = {depth = 0; locals = Map_.empty; self = None}

let show_env env = show_env {env with locals = env.locals |> Map_.remove "std" |> Map_.remove "$"}

let fields (_,{fields;_},_) = fields