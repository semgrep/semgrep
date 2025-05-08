(*****************************************************************************)
(* Pervasive types and operators *)
(*****************************************************************************)

val glob : Fpath.t -> Fpath.t list
(** [glob glob_path] is a list of paths of the files matching a path containing wildcards
  * i.e. [glob (Fpath.v "dir/**/*.extension")] is the list of files in the [dir] directory
  * or its subdirectories ending in .extension. This function is analogous
  * to the "ls pattern" in the shell.
  *)

val unix_diff : string -> string -> string list

(*****************************************************************************)
(* Test. But have a look at ounit.mli *)
(*****************************************************************************)

(* regression testing *)
type score_result = Ok | Pb of string
type score = (string (* usually a filename *), score_result) Hashtbl.t
type score_list = (string (* usually a filename *) * score_result) list

val empty_score : unit -> score

val regression_testing :
  score -> string (* old score file on disk (usually in /tmp) *) -> unit

val regression_testing_vs : score -> score -> score
val total_scores : score -> int (* good *) * int (* total *)
val print_score : score -> unit
val print_total_score : score -> unit

(*****************************************************************************)
(* String_of and (pretty) printing *)
(*****************************************************************************)

val string_of_list : ('a -> string) -> 'a list -> string
val string_of_option : ('a -> string) -> 'a option -> string

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)

val error_cant_have : 'a -> 'b

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

val string_of_char : char -> string
val string_of_chars : char list -> string

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

val chop : string -> string

(* strip c s removes all contiguous prefixes of [c] from [s]
   e.g. strip 'a' "abc"   = "bc"
        strip 'b' "abc"   = "abc"
        strip 'c' "cabcc" = "ab"
*)
val strip : char -> string -> string

(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)

val string_match_substring : Str.regexp -> string -> bool
val all_match : string (* regexp *) -> string -> string list

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

val inits_of_relative_dir : Fpath.t -> Fpath.t list

(*****************************************************************************)
(* Lines/Words/Strings *)
(*****************************************************************************)

val list_of_string : string -> char list
val nblines_eff : string -> int

(*###########################################################################*)
(* Collection-like types *)
(*###########################################################################*)

(*****************************************************************************)
(* Nonempty List *)
(*****************************************************************************)

type 'a nonempty = Nonempty of 'a * 'a list

val nonempty_to_list : 'a nonempty -> 'a list

(*****************************************************************************)
(* List *)
(*****************************************************************************)

val inits : 'a list -> 'a list list

val uniq : 'a list -> 'a list
(** Not like unix uniq command line tool that only delete contiguous repeated
   line. Here we delete any repeated line (here list element).
 *)

val map_flatten : ('a -> 'b list) -> 'a list -> 'b list
val maximum : 'a list -> 'a
val minimum : 'a list -> 'a
val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a
val sum_float : float list -> float
val sum_int : int list -> int

val group : ('a -> 'a -> bool) -> 'a list -> 'a nonempty list
(** Groups a list into a list of equivalence classes (themselves nonempty
   lists) according to the given equality predicate. `eq` must be an
   equivalence relation for correctness.
 *)

val repeat : 'a -> int -> 'a list
val head_middle_tail : 'a list -> 'a * 'a list * 'a
val list_last : 'a list -> 'a
val splitAt : int -> 'a list -> 'a list * 'a list
val split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list
val split_gen_when : ('a list -> 'a list option) -> 'a list -> 'a list list
val group_by_mapped_key : ('a -> 'b) -> 'a list -> ('b * 'a list) list
val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list

(*****************************************************************************)
(* Set. But have a look too at set*.mli; it's better. Or use Hashtbl. *)
(*****************************************************************************)

(* use internally a hash and return
 * - the common part,
 * - part only in a,
 * - part only in b
 *)
val diff_set_eff : 'a list -> 'a list -> 'a list * 'a list * 'a list

(*****************************************************************************)
(* N-ary tree *)
(*****************************************************************************)

val dirs_and_base_of_file : string -> string list * string
