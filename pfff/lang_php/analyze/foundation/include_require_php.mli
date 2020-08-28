(*s: include_require_php.mli *)
(*x: include_require_php.mli *)
type increq = 
  increq_kind * Cst_php.tok * increq_expr 

 and increq_expr = 
   | Direct of Common.filename 
   | ConcatVar of Cst_php.dname * Common.filename
   | ConcatConstant of Cst_php.ident * Common.filename
   | ConcatArrrayVar of Cst_php.dname * string * Common.filename
   | ConcatDirname of Common.filename
   | ConcatRealpathDirname of Common.filename

   | SimpleVar of Cst_php.dname
   | Other of Cst_php.expr

 and increq_kind = 
   | Include
   | IncludeOnce
   | Require
   | RequireOnce

val top_increq_of_program: Cst_php.program -> increq list
val all_increq_of_any: Cst_php.any -> increq list
val increq_expr_of_expr: Cst_php.expr -> increq_expr

val resolve_path: 
  Env_php.env * Common.dirname (* pwd of file *) -> increq_expr -> 
  Common.filename option

val includes_of_file: 
  Env_php.env -> Common.filename -> Common.filename list

(* The hook below is to let reuse this code by having another "includers"
 * preprocessor, for instance one that understand facebook flib specificities
 * like require_module() directives.
 * 
 * This functions helps doing what gcc does by first calling 'cpp' on a file
 * to get all information.
 *)
val recursive_included_files_of_file:
 ?verbose:bool -> ?depth_limit: int option ->
 ?includes_of_file:(Env_php.env -> Common.filename -> Common.filename list) ->
 Env_php.env -> Common.filename -> Common.filename list

(*e: include_require_php.mli *)
