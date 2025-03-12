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
(* Debugging/logging *)
(*****************************************************************************)

val _tab_level_print : int ref
val indent_do : (unit -> 'a) -> 'a
val reset_pr_indent : unit -> unit

(* The following functions first indent _tab_level_print spaces.
 * They also add the _prefix_pr, for instance used in MPI to show which
 * worker is talking.
 * update: for pr2, it can also print into a log file.
 *
 * The use of 2 in pr2 is because 2 is under UNIX the second descriptor
 * which corresponds to stderr.
 *)
val _prefix_pr : string ref
val pr_no_nl : string -> unit
val pr_xxxxxxxxxxxxxxxxx : unit -> unit

(* pr2 print on stderr, but can also in addition print into a file *)
val _chan_pr2 : out_channel option ref
val pr2_no_nl : string -> unit
val pr2_xxxxxxxxxxxxxxxxx : unit -> unit

(* use Dumper.dump *)
val mk_pr2_wrappers : bool ref -> (string -> unit) * (string -> unit)
val redirect_stdout_opt : string option -> (unit -> 'a) -> 'a
val redirect_stdout_stderr : string -> (unit -> unit) -> unit
val redirect_stdin : string -> (unit -> unit) -> unit
val redirect_stdin_opt : string option -> (unit -> unit) -> unit
val print_n : int -> string -> unit
val printerr_n : int -> string -> unit

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

val string_of_string : (string -> string) -> string
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_unit : unit -> string
val string_of_array : ('a -> string) -> 'a array -> string
val string_of_option : ('a -> string) -> 'a option -> string
val print_bool : bool -> unit
val print_option : ('a -> unit) -> 'a option -> unit
val print_list : ('a -> unit) -> 'a list -> unit
val print_between : (unit -> unit) -> ('a -> unit) -> 'a list -> unit

(* use Format internally *)
val pp_do_in_box : (unit -> unit) -> unit
val pp_f_in_box : (unit -> 'a) -> 'a
val pp_do_in_zero_box : (unit -> unit) -> unit
val pp : string -> unit

(* works with _tab_level_print enabling to mix some calls to pp, pr2
 * and indent_do to sometimes use advanced indentation pretty printing
 * (with the pp* functions) and sometimes explicit and simple indendation
 * printing (with pr* and indent_do) *)
val adjust_pp_with_indent : (unit -> unit) -> unit
val adjust_pp_with_indent_and_header : string -> (unit -> unit) -> unit

val mk_str_func_of_assoc_conv :
  ('a * string) list -> (string -> 'a) * ('a -> string)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

val ( +!> ) : 'a ref -> ('a -> 'a) -> unit
val ( $ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val forever : (unit -> unit) -> unit

class ['a] shared_variable_hook : 'a -> object
  val mutable data : 'a
  val mutable registered : (unit -> unit) list
  method get : 'a
  method modify : ('a -> 'a) -> unit
  method register : (unit -> unit) -> unit
  method set : 'a -> unit
end

val fixpoint : ('a -> 'a) -> 'a -> 'a

val fixpoint_for_object :
  ((< equal : 'a -> bool ; .. > as 'a) -> 'a) -> 'a -> 'a

val add_hook : ('a -> ('a -> 'b) -> 'b) ref -> ('a -> ('a -> 'b) -> 'b) -> unit
val add_hook_action : ('a -> unit) -> ('a -> unit) list ref -> unit
val run_hooks_action : 'a -> ('a -> unit) list ref -> unit

type 'a mylazy = unit -> 'a

(* emacs spirit *)
val save_excursion_and_disable : bool ref -> (unit -> 'b) -> 'b
val save_excursion_and_enable : bool ref -> (unit -> 'b) -> 'b
val cache_in_ref : 'a option ref -> (unit -> 'a) -> 'a
val oncef : ('a -> unit) -> 'a -> unit
val once : bool ref -> (unit -> unit) -> unit
val before_leaving : ('a -> unit) -> 'a -> 'a

(* cf also the timeout function below that are control related too *)

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

type 'a set = 'a list

val empty_set : 'a set
val insert_set : 'a -> 'a set -> 'a set
val single_set : 'a -> 'a set
val set : 'a list -> 'a set
val exists_set : ('a -> bool) -> 'a set -> bool
val forall_set : ('a -> bool) -> 'a set -> bool
val filter_set : ('a -> bool) -> 'a set -> 'a set
val fold_set : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a
val map_set : ('a -> 'b) -> 'a set -> 'b set
val member_set : 'a -> 'a set -> bool
val find_set : ('a -> bool) -> 'a list -> 'a
val sort_set : ('a -> 'a -> int) -> 'a list -> 'a list
val iter_set : ('a -> unit) -> 'a list -> unit
val top_set : 'a set -> 'a
val inter_set : 'a set -> 'a set -> 'a set
val union_set : 'a set -> 'a set -> 'a set
val minus_set : 'a set -> 'a set -> 'a set
val union_all : 'a set list -> 'a set
val big_union_set : ('a -> 'b set) -> 'a set -> 'b set
val card_set : 'a set -> int
val include_set : 'a set -> 'a set -> bool
val equal_set : 'a set -> 'a set -> bool
val include_set_strict : 'a set -> 'a set -> bool

(* could put them in Common.Infix *)
val ( $*$ ) : 'a set -> 'a set -> 'a set
val ( $+$ ) : 'a set -> 'a set -> 'a set
val ( $-$ ) : 'a set -> 'a set -> 'a set
val ( $?$ ) : 'a -> 'a set -> bool
val ( $<$ ) : 'a set -> 'a set -> bool
val ( $<=$ ) : 'a set -> 'a set -> bool
val ( $=$ ) : 'a set -> 'a set -> bool
val ( $@$ ) : 'a list -> 'a list -> 'a list
val nub : 'a list -> 'a list

(* use internally a hash and return
 * - the common part,
 * - part only in a,
 * - part only in b
 *)
val diff_set_eff : 'a list -> 'a list -> 'a list * 'a list * 'a list

(*****************************************************************************)
(* Set as normal list *)
(*****************************************************************************)

(* cf above *)

(*****************************************************************************)
(* Set as sorted list *)
(*****************************************************************************)

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
(* Assoc. But have a look too at Mapb.mli; it's better. Or use Hashtbl. *)
(*****************************************************************************)

type ('a, 'b) assoc = ('a * 'b) list

val assoc_to_function : (* Eq a *) ('a, 'b) assoc -> 'a -> 'b
val empty_assoc : ('a, 'b) assoc
val fold_assoc : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val insert_assoc : 'a -> 'a list -> 'a list
val map_assoc : ('a -> 'b) -> 'a list -> 'b list
val filter_assoc : ('a -> bool) -> 'a list -> 'a list
val assoc : 'a -> ('a * 'b) list -> 'b
val keys : ('a * 'b) list -> 'a list
val lookup : 'a -> ('a * 'b) list -> 'b
val del_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val replace_assoc : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
val apply_assoc : 'a -> ('b -> 'b) -> ('a * 'b) list -> ('a * 'b) list
val big_union_assoc : ('a -> 'b set) -> 'a list -> 'b set
val assoc_reverse : ('a * 'b) list -> ('b * 'a) list
val assoc_map : ('a * 'b) list -> ('a * 'b) list -> ('a * 'a) list
val lookup_list : 'a -> ('a, 'b) assoc list -> 'b
val lookup_list2 : 'a -> ('a, 'b) assoc list -> 'b * int
val assoc_opt : 'a -> ('a, 'b) assoc -> 'b option
val assoc_with_err_msg : 'a -> ('a, 'b) assoc -> 'b

type order = HighFirst | LowFirst

val compare_order : order -> 'a -> 'a -> int
val sort_by_val_lowfirst : ('a, 'b) assoc -> ('a * 'b) list
val sort_by_val_highfirst : ('a, 'b) assoc -> ('a * 'b) list
val sort_by_key_lowfirst : ('a, 'b) assoc -> ('a * 'b) list
val sort_by_key_highfirst : ('a, 'b) assoc -> ('a * 'b) list
val sortgen_by_key_lowfirst : ('a, 'b) assoc -> ('a * 'b) list
val sortgen_by_key_highfirst : ('a, 'b) assoc -> ('a * 'b) list

(*****************************************************************************)
(* Assoc, specialized. *)
(*****************************************************************************)

module IntMap : sig
  type key = int
  type +'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

val intmap_to_list : 'a IntMap.t -> (IntMap.key * 'a) list
val intmap_string_of_t : 'a -> 'b -> string

module IntIntMap : sig
  type key = int * int
  type +'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

val intintmap_to_list : 'a IntIntMap.t -> (IntIntMap.key * 'a) list
val intintmap_string_of_t : 'a -> 'b -> string

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

(* Note that Hashtbl keep old binding to a key so if want a hash
 * of a list, then can use the Hashtbl as is. Use Hashtbl_.get_stack then
 * to get the list of bindings
 *
 * Note that Hashtbl module use different convention :( the object is
 * the first argument, not last as for List or Map.
 *)

(* obsolete: can use directly the Hashtbl module *)
val hcreate : unit -> ('a, 'b) Hashtbl.t
val hadd : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hmem : 'a -> ('a, 'b) Hashtbl.t -> bool
val hfind : 'a -> ('a, 'b) Hashtbl.t -> 'b
val hreplace : 'a * 'b -> ('a, 'b) Hashtbl.t -> unit
val hiter : ('a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
val hfold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) Hashtbl.t -> 'c -> 'c
val hremove : 'a -> ('a, 'b) Hashtbl.t -> unit
val hfind_default : 'a -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> 'b
val hfind_option : 'a -> ('a, 'b) Hashtbl.t -> 'b option

val hupdate_default :
  'a -> update:('b -> 'b) -> default:(unit -> 'b) -> ('a, 'b) Hashtbl.t -> unit

val add1 : int -> int
val cst_zero : unit -> int
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hash_to_list_unsorted : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hkeys : ('a, 'b) Hashtbl.t -> 'a list

(* hunion h1 h2  adds all binding in h2 into h1 *)
val hunion : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> unit

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = 'a Hashtbl_.hashset

(* common use of hashset, in a hash of hash *)
val hash_hashset_add : 'a -> 'b -> ('a, 'b hashset) Hashtbl.t -> unit

(* hashset_union h1 h2  adds all elements in h2 into h1 *)
val hashset_union : 'a hashset -> 'a hashset -> unit

(* hashset_inter h1 h2  removes all elements in h1 not in h2 *)
val hashset_inter : 'a hashset -> 'a hashset -> unit

val hashset_to_set :
  < fromlist : 'a list -> 'c ; .. > -> ('a, 'b) Hashtbl.t -> 'c

(*****************************************************************************)
(* Hash  with default value *)
(*****************************************************************************)
type ('a, 'b) hash_with_default =
  < add : 'a -> 'b -> unit
  ; to_list : ('a * 'b) list
  ; to_h : ('a, 'b) Hashtbl.t
  ; update : 'a -> ('b -> 'b) -> unit
  ; assoc : 'a -> 'b >

val hash_with_default :
  (unit -> 'b) ->
  < add : 'a -> 'b -> unit
  ; to_list : ('a * 'b) list
  ; to_h : ('a, 'b) Hashtbl.t
  ; update : 'a -> ('b -> 'b) -> unit
  ; assoc : 'a -> 'b >

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
