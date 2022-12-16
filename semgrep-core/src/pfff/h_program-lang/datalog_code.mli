
type var = string
type func = string
type fld = string

type heap = string
type callsite = string

type fact =
  | PointTo of var * heap

  | Assign of var * var
  | AssignContent of var * var
  | AssignAddress of var * var

  | AssignDeref of var * var

  | AssignLoadField of var * var * fld
  | AssignStoreField of var * fld * var
  | AssignFieldAddress of var * var * fld

  | AssignArrayElt of var * var
  | AssignArrayDeref of var * var
  | AssignArrayElementAddress of var * var

  | Parameter of func * int * var
  | Return of func * var (* ret_xxx convention *)
  | Argument of callsite * int * var
  | ReturnValue of callsite * var
  | CallDirect of callsite * func
  | CallIndirect of callsite * var

(* for toy datalog *)
val string_of_fact:
  fact -> string

val bddbddb_of_facts:
  fact list -> Common.dirname -> unit

(* from a .tuples to a .explain *)
val bddbddb_explain_tuples:
  Common.filename -> Common.filename
