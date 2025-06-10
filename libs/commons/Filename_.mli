(* Deprecated: use the Ppath module instead! *)
val readable : root:Fpath.t -> Fpath.t -> Fpath.t
(** [readable ~root p] finds a "readable" path to [p] relative to
    the [root] (this is generally the root of a project). *)

val dbe_of_filename : string -> string * string * string
(** [dbe_of_filename "a/b/c.ext1.ext2"] evaluates to [("a/b","c","ext1.ext2")]
    if the file didn't have an extention [dbe_of_filename "a/b/c"] it
    evaluates to [("a/b","c","")]
 *)

val filename_of_dbe : string * string * string -> string
(** [String.equal (filename_of_dbe (dbe_of_filename file)) file] is *)

val dbe_of_filename_many_ext_opt : string -> (string * string * string) option
(** [dbe_of_filename_many_ext_opt filename] returns [Some (d,b,e)], where
 * [d] is the directory path, and [b ^ "." ^ e] is the base name, where
 * [b] contains no period '.' characters. If this split is not possible,
 * the result is [None].
 * E.g.:
 *     dbe_of_filename_many_ext_opt "foo.test.yaml" = Some (".", "foo", "test.yaml")
 *     dbe_of_filename_many_ext_opt "foo"           = None
 *)
