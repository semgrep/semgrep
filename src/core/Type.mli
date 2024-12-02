(* Types for type inference.
 *
 * 'r below stands for 'resolved
 *)

type todo_kind = string option

(* Fully qualified name *)
and 'r name = 'r * 'r type_argument list

and 'r type_argument =
  | TA of 'r t
  (* Java: `?`, `? extends Foo`, `? super Foo` *)
  | TAWildcard of 'r type_arg_constraint option
  | OtherTypeArg of todo_kind

and 'r type_arg_constraint =
  (* Java: `? extends Foo` *)
  | TAUpper of 'r t
  (* Java: `? super Foo` *)
  | TALower of 'r t

and 'r t =
  | N of 'r name * (* alt names *) AST_generic.alternate_name list
  (* e.g. for unresolved types in core libraries*)
  | UnresolvedName of string * 'r type_argument list
  | Builtin of builtin_type
  (* for null analysis *)
  | Null
  (* int option for the cases where we know the size of the array *)
  | Array of Parsed_int.t option * 'r t
  | Function of 'r function_type
  | Pointer of 'r t
  (* NoType is to avoid some Type.t option and use of let* everywhere.
   * See also of_opt() below.
   *)
  | NoType
  | Todo of todo_kind

and builtin_type =
  (* mimic SAST.literal *)
  | Int
  | Float
  | String
  | Bool
  | Number
  | OtherBuiltins of string

and 'r function_type = 'r parameter list * 'r t
and 'r parameter = Param of 'r parameter_classic | OtherParam of todo_kind

and 'r parameter_classic = {
  (* the identifier can be useful to handle ArgKwd calls *)
  pident : string option;
  ptype : 'r t;
}
[@@deriving show, eq, sexp]

(* match None to NoType *)
val of_opt : 'r t option -> 'r t
val to_name_opt : Lang.t -> 'r t -> 'r name option

(* true unless the type is NoType or Todo *)
val is_real_type : 'r t -> bool

val to_ast_generic_type_ :
  ?tok:Tok.t ->
  Lang.t ->
  ('r -> AST_generic.alternate_name list -> AST_generic.name) ->
  'r t ->
  AST_generic.type_ option

(* note that Lang.t is currently not used *)
val builtin_type_of_string : Lang.t -> string -> builtin_type option
val builtin_type_of_type : Lang.t -> AST_generic.type_ -> builtin_type option
val name_of_builtin_type : Lang.t -> builtin_type -> string
val todo_parameter : 'a parameter

class virtual ['c] map : object ('c)
  constraint
  'c = < visit_'resolved : 'd -> 'g -> 'h
       ; visit_Array : 'd -> Parsed_int.t option -> 'g t -> 'h t
       ; visit_Bool : 'd -> builtin_type
       ; visit_Builtin : 'd -> builtin_type -> 'h t
       ; visit_Float : 'd -> builtin_type
       ; visit_Function : 'd -> 'g function_type -> 'h t
       ; visit_Int : 'd -> builtin_type
       ; visit_N : 'd -> 'g name -> AST_generic.alternate_name list -> 'h t
       ; visit_NoType : 'd -> 'h t
       ; visit_Null : 'd -> 'h t
       ; visit_Number : 'd -> builtin_type
       ; visit_OtherBuiltins : 'd -> string -> builtin_type
       ; visit_OtherParam : 'd -> todo_kind -> 'h parameter
       ; visit_OtherTypeArg : 'd -> todo_kind -> 'h type_argument
       ; visit_Param : 'd -> 'g parameter_classic -> 'h parameter
       ; visit_Pointer : 'd -> 'g t -> 'h t
       ; visit_String : 'd -> builtin_type
       ; visit_TA : 'd -> 'g t -> 'h type_argument
       ; visit_TALower : 'd -> 'g t -> 'h type_arg_constraint
       ; visit_TAUpper : 'd -> 'g t -> 'h type_arg_constraint
       ; visit_TAWildcard :
           'd -> 'g type_arg_constraint option -> 'h type_argument
       ; visit_Todo : 'd -> todo_kind -> 'h t
       ; visit_UnresolvedName : 'd -> string -> 'g type_argument list -> 'h t
       ; visit_alternate_name :
           'd -> AST_generic.alternate_name -> AST_generic.alternate_name
       ; visit_builtin_type : 'd -> builtin_type -> builtin_type
       ; visit_function_type :
           'd -> 'g function_type -> 'h parameter list * 'h t
       ; visit_name : 'd -> 'g name -> 'h * 'h type_argument list
       ; visit_parameter : 'd -> 'g parameter -> 'h parameter
       ; visit_parameter_classic :
           'd -> 'g parameter_classic -> 'h parameter_classic
       ; visit_parsed_int : 'd -> Parsed_int.t -> Parsed_int.t
       ; visit_t : 'd -> 'g t -> 'h t
       ; visit_todo_kind : 'd -> todo_kind -> todo_kind
       ; visit_type_arg_constraint :
           'd -> 'g type_arg_constraint -> 'h type_arg_constraint
       ; visit_type_argument : 'd -> 'g type_argument -> 'h type_argument
       ; .. >

  method virtual visit_'resolved : 'd -> 'g -> 'h
  method visit_Array : 'd -> Parsed_int.t option -> 'g t -> 'h t
  method visit_Bool : 'd -> builtin_type
  method visit_Builtin : 'd -> builtin_type -> 'h t
  method visit_Float : 'd -> builtin_type
  method visit_Function : 'd -> 'g function_type -> 'h t
  method visit_Int : 'd -> builtin_type
  method visit_N : 'd -> 'g name -> AST_generic.alternate_name list -> 'h t
  method visit_NoType : 'd -> 'h t
  method visit_Null : 'd -> 'h t
  method visit_Number : 'd -> builtin_type
  method visit_OtherBuiltins : 'd -> string -> builtin_type
  method visit_OtherParam : 'd -> todo_kind -> 'h parameter
  method visit_OtherTypeArg : 'd -> todo_kind -> 'h type_argument
  method visit_Param : 'd -> 'g parameter_classic -> 'h parameter
  method visit_Pointer : 'd -> 'g t -> 'h t
  method visit_String : 'd -> builtin_type
  method visit_TA : 'd -> 'g t -> 'h type_argument
  method visit_TALower : 'd -> 'g t -> 'h type_arg_constraint
  method visit_TAUpper : 'd -> 'g t -> 'h type_arg_constraint

  method visit_TAWildcard :
    'd -> 'g type_arg_constraint option -> 'h type_argument

  method visit_Todo : 'd -> todo_kind -> 'h t
  method visit_UnresolvedName : 'd -> string -> 'g type_argument list -> 'h t

  method visit_alternate_name :
    'd -> AST_generic.alternate_name -> AST_generic.alternate_name

  method private visit_array :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

  method private visit_bool : 'env. 'env -> bool -> bool
  method visit_builtin_type : 'd -> builtin_type -> builtin_type
  method private visit_bytes : 'env. 'env -> bytes -> bytes
  method private visit_char : 'env. 'env -> char -> char
  method private visit_float : 'env. 'env -> float -> float

  method visit_function_type :
    'd -> 'g function_type -> 'h parameter list * 'h t

  method private visit_int : 'env. 'env -> int -> int
  method private visit_int32 : 'env. 'env -> int32 -> int32
  method private visit_int64 : 'env. 'env -> int64 -> int64

  method private visit_lazy_t :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t

  method private visit_list :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

  method visit_name : 'd -> 'g name -> 'h * 'h type_argument list
  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

  method private visit_option :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

  method visit_parameter : 'd -> 'g parameter -> 'h parameter

  method visit_parameter_classic :
    'd -> 'g parameter_classic -> 'h parameter_classic

  method visit_parsed_int : 'd -> Parsed_int.t -> Parsed_int.t

  method private visit_ref :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

  method private visit_result :
    'env 'a 'b 'e 'f.
    ('env -> 'a -> 'b) ->
    ('env -> 'e -> 'f) ->
    'env ->
    ('a, 'e) Result.result ->
    ('b, 'f) Result.result

  method private visit_string : 'env. 'env -> string -> string
  method visit_t : 'd -> 'g t -> 'h t
  method visit_todo_kind : 'd -> todo_kind -> todo_kind

  method visit_type_arg_constraint :
    'd -> 'g type_arg_constraint -> 'h type_arg_constraint

  method visit_type_argument : 'd -> 'g type_argument -> 'h type_argument
  method private visit_unit : 'env. 'env -> unit -> unit
end
