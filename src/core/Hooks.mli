val exit : (unit -> unit) list ref
val get_type : (AST_generic.ident -> AST_generic.type_ option) ref
val get_def : (AST_generic.ident -> string (* filename *) option) ref
