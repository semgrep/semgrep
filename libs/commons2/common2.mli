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
(* Concurrency *)
(*****************************************************************************)

(* how ensure really atomic file creation ? hehe :) *)
exception FileAlreadyLocked

val acquire_file_lock : string -> unit
val release_file_lock : string -> unit

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)

val error_cant_have : 'a -> 'b

(*###########################################################################*)
(* Basic types *)
(*###########################################################################*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)

val ( ||| ) : 'a -> 'a -> 'a
val ( ==> ) : bool -> bool -> bool
val xor : 'a -> 'a -> bool

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

val string_of_char : char -> string
val string_of_chars : char list -> string

(*****************************************************************************)
(* Random *)
(*****************************************************************************)

(* val _init_random : unit *)
val random_list : 'a list -> 'a
val random_subset_of_list : int -> 'a list -> 'a list

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

(*****************************************************************************)
(* Maybe *)
(*****************************************************************************)

val just : 'a option -> 'a
val some : 'a option -> 'a (* alias *)
val optionise : (unit -> 'a) -> 'a option
val some_or : 'a option -> 'a -> 'a
val option_to_list : 'a option -> 'a list
val list_to_single_or_exn : 'a list -> 'a
val while_some : gen:(unit -> 'a option) -> f:('a -> 'b) -> unit -> 'b list
val ( ||= ) : 'a option ref -> (unit -> 'a) -> unit
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( |? ) : 'a option -> 'a Lazy.t -> 'a

(*****************************************************************************)
(* TriBool *)
(*****************************************************************************)
type bool3 = True3 | False3 | TrueFalsePb3 of string

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
val lines : string -> string list
val unlines : string list -> string
val words : string -> string list
val unwords : string list -> string
val split_space : string -> string list
val lines_with_nl : string -> string list
val nblines : string -> int
val nblines_eff : string -> int
val words_of_string_with_newlines : string -> string list

(* e.g. on "ab\n\nc" it will return [Left "ab"; Right (); Right (); Left "c"] *)
val lines_with_nl_either : string -> (string, unit) Either.t list

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
(* Sets specialized *)
(*****************************************************************************)

module StringSet : sig
  type elt = string
  type t

  val empty : t
  val add : string -> t -> t
  val remove : string -> t -> t
  val singleton : string -> t
  val of_list : string list -> t
  val to_list : t -> string list
  val is_empty : t -> bool
  val mem : string -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val iter : (string -> unit) -> t -> unit
  val fold : (string -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (string -> bool) -> t -> bool
  val exists : (string -> bool) -> t -> bool
  val filter : (string -> bool) -> t -> t
  val partition : (string -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> string list
  (*
        val min_string : t -> string
        val max_string : t -> string
      *)

  val choose : t -> string
  val split : string -> t -> t * bool * t
end

(*****************************************************************************)
(* N-ary tree *)
(*****************************************************************************)

type ('a, 'b) tree = Node of 'a * ('a, 'b) tree list | Leaf of 'b

val dirs_and_base_of_file : string -> string list * string
val tree_of_files : string list -> (string, string * string) tree

(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)

(* mostly alias to functions in List_ *)

val map : ('a -> 'b) -> 'a list -> 'b list
