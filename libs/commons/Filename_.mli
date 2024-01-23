(* Deprecated: use the Ppath module instead! *)
val filename_without_leading_path :
  string -> string (* filename *) -> string (* filename *)

val readable : root:string -> string (* filename *) -> string (* filename *)

(* stuff that was in common2.mli *)

val filesuffix : string -> string
val fileprefix : string -> string

(* db for dir, base *)
val db_of_filename : string -> string * string
val filename_of_db : string * string -> string

(* dbe for dir, base, ext *)
val dbe_of_filename : string -> string * string * string
val dbe_of_filename_nodot : string -> string * string * string

(* Left (d,b,e) | Right (d,b)  if file has no extension *)
val dbe_of_filename_safe :
  string -> (string * string * string, string * string) Either.t

val dbe_of_filename_noext_ok : string -> string * string * string

(* [dbe_of_filename_many_ext_opt filename] returns [Some (d,b,e)], where
 * [d] is the directory path, and [b ^ "." ^ e] is the base name, where
 * [b] contains no period '.' characters. If this split is not possible,
 * the result is [None].
 * E.g.:
 *     dbe_of_filename_many_ext_opt "foo.test.yaml" = Some (".", "foo", "test.yaml")
 *     dbe_of_filename_many_ext_opt "foo"           = None
 *)
val dbe_of_filename_many_ext_opt : string -> (string * string * string) option
val filename_of_dbe : string * string * string -> string
