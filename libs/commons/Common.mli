(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module contains functions (and types) which are very often used.
 * They are so common (hence the name of this file) that lots of modules
 * just 'open Common' to get in scope those functions.
 * This file acts like a second Stdlib.ml (used to be called Pervasives.ml).
 *
 * However, because this module is often open'ed, it should
 * not define too many functions (<100) because we can't impose
 * to other programmers the mental effort to know too many functions.
 * This was actually a big problem with the first version of Common.ml
 * (renamed to Common2.ml since).
 *)

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

(* You should not use the polymorphic '='. It is convenient but
 * its use will eventually backfire. You should use instead 'deriving eq'
 * where the equality function can be customized.
 * To enforce this rule, this module redefines '=' to just operate
 * on strings, so ocaml can statically detect when you wrongly use '='
 * on other types.
 *
 * See also the Operators submodule at the end of this file.
 *)
val ( = ) : string -> string -> bool

(* If you need to use '=', at least use the more precise operators below. *)
val ( =|= ) : int -> int -> bool
val ( =$= ) : char -> char -> bool
val ( =:= ) : bool -> bool -> bool

(* if you really really need to use the polymorphic '=', at least use
 * the operator below so it's easier to grep for it if one needs to refactor
 * the code to use 'deriving eq' instead.
 *)
val ( =*= ) : 'a -> 'a -> bool

(*****************************************************************************)
(* Printing/debugging *)
(*****************************************************************************)
(* see also Dumper.ml *)

(* Same as print_endline: print the string and a newline, then flush stdout.
 * Just shorter. *)
val pr : string -> unit

(* Print a string and a newline to stderr, then flush stderr. The '2'
 * is used to refect that it prints on stderr (file descriptor '2' in Unix). *)
val pr2 : string -> unit

(* Print on stderr any data structure (using Dumper.dump) *)
val pr2_gen : 'a -> unit

(* Print on stderr but only once (to avoid printing the same error
 * again and again) *)
val pr2_once : string -> unit

(* forbid pr2_once to do the once "optimisation" *)
val _already_printed : (string, bool) Hashtbl.t
val disable_pr2_once : bool ref

(* to be used in pipes as in foo() |> before_return (fun v -> pr2_gen v)*)
val before_return : ('a -> unit) -> 'a -> 'a

(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)
(* see also Exception.ml functions as well as Time_limit.Timeout
 * in the process_limits library.
 *)

exception Todo

(* some people prefer assert false *)
exception Impossible

(* similar to Not_found but to use when something returns too many findings *)
exception Multi_found

(* You should use this instead of 'exit n' because it allows you
 * to intercept the exn and do something special before exiting.
 *)
exception UnixExit of int

(* Convert any exception to a string *)
val exn_to_s : exn -> string

(* if set then certain functions like unwind_protect will not
 * do a try and finalize and instead just call the function, which
 * helps in ocamldebug and also in getting better backtraces.
 * This is also useful to set in a js_of_ocaml (jsoo) context to
 * again get better backtraces.
 *)
val debugger : bool ref

(* Emacs-inspired finalize-like function. *)
val unwind_protect : (unit -> 'a) -> (Exception.t -> unit) -> 'a
val save_excursion : 'a ref -> 'a -> (unit -> 'b) -> 'b

(* Java-inspired combinator *)
val finalize : (unit -> 'a) -> (unit -> unit) -> 'a

(*****************************************************************************)
(* Strings and regexps *)
(*****************************************************************************)

(* shortcuts for string_of_int and int_of_string *)
val i_to_s : int -> string
val s_to_i : string -> int
val null_string : string -> bool
val contains : string -> string -> bool

(* Shortcut for Printf.sprintf *)
val spf : ('a, unit, string) format -> 'a

(* Perl-like regexp pattern matching. We need the many matchedxxx()
 * because OCaml does not support polytypic functions (same problem
 * with zip1/zip2/etc.).
 * Here is how to use =~ and matchedxxx together:
 *    let s = "foobar" in
 *    if s =~ "f\\(..\\)\\(.*\\)"
 *    then
 *      let after_f, endpart = Common.matched2 s in
 *      ...
 *)
val ( =~ ) : string -> string -> bool
val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 : string -> string * string * string * string * string * string

val matched7 :
  string -> string * string * string * string * string * string * string

(* join/split strings *)
val join : string (* sep *) -> string list -> string
val split : string (* sep regexp *) -> string -> string list

(*****************************************************************************)
(* Real file paths - deprecated, use File.mli *)
(*****************************************************************************)
(* Deprecated!

   Migration in progress: File.ml reproduces the functions below and uses
   Fpath.t instead of strings to represent file/directory paths.
*)

(* Some signatures are arguably clearer when using 'filename' instead of
 * 'string' (see write_file below for an example).
 * Ideally 'filename' should be a different type like in Scala with the
 * Path module: https://www.lihaoyi.com/post/HowtoworkwithFilesinScala.html
 *)
type filename = string [@@deriving show, eq, ord, sexp]

(*
   Check that the file exists and produce a valid absolute path for the file.
   Deprecated: use the Rpath module instead!
*)
val fullpath : filename -> filename

(* Deprecated: use the Ppath module instead! *)
val filename_without_leading_path : string -> filename -> filename
val readable : root:string -> filename -> filename
val dir_contents : filename -> filename list

val dir_contents : filename -> filename list
(** [dir_contents dir] will return a recursive list of all files in a directory *)

(* use the command 'find' internally and tries to skip files in
 * version control system (vcs) (e.g., .git, _darcs, etc.).
 * Deprecated?
 *)
val files_of_dir_or_files_no_vcs_nofilter : string list -> filename list

(* ugly: internal flag for files_of_dir_or_files_no_vcs_nofilter *)
val follow_symlinks : bool ref

(*****************************************************************************)
(* IO - deprecated, use File.mli *)
(*****************************************************************************)

(* Inputs a line of text in a platform-agnostic way. Should be preferred over
   `input_line`, especially when dealing with Windows.
   More info can be found in `Common.ml`.
   This in-channel should be opened in binary mode.
*)
val input_text_line : in_channel -> string

(*
   Return the lines of a file. Both Windows-style and Unix-style line endings
   are recognized and removed from the end of the line.
*)
val cat : filename -> string list
val write_file : file:filename -> string -> unit

(* Read the contents of file.

   This implementation works even with Linux files like /dev/fd/63
   created by bash when using "process substitution"* e.g.

     my-ocaml-program <(echo contents)

   * https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   If max_len is specified, at most that many bytes are read from the file.
*)
val read_file : ?max_len:int -> filename -> string

(* Scheme-inspired combinators that automatically close the file
 * once the function callback is done. Here is an example of use:
 *   with_open_outfile "/tmp/foo.txt" (fun (pr, _chan) ->
 *     pr "this goes in foo.txt"
 *   )
 *)
val with_open_outfile : filename -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_infile : filename -> (in_channel -> 'a) -> 'a

(* creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c"
 *)
val new_temp_file : string (* prefix *) -> string (* suffix *) -> filename

(* ??? *)
val _temp_files_created : (string, unit) Hashtbl.t
val save_tmp_files : bool ref
val erase_temp_files : unit -> unit
val erase_this_temp_file : filename -> unit

(*****************************************************************************)
(* Subprocess *)
(*****************************************************************************)
(* Deprecated? Use Bos instead? *)

(* This allows to capture the output of an external command.
 * It is more convenient to use than Unix.open_process_in.
 * Here is an example of use:
 *  let (lines, _status) = cmd_to_list_and_status "find /home" in
 *  ...
 *)
val cmd_to_list_and_status :
  ?verbose:bool -> string -> string list * Unix.process_status

exception CmdError of Unix.process_status * string

(* this may raise CmdError *)
val cmd_to_list : ?verbose:bool -> string -> string list (* alias *)

(*****************************************************************************)
(* Lists *)
(*****************************************************************************)

(* Shortcut for xs =*= [].
 * It is not that shorter, but it avoids to use =*= which helps
 * to reduce the number of places to look for possible problems with =*=.
 *)
val null : 'a list -> bool

val map : ('a -> 'b) -> 'a list -> 'b list
(** Same as [List.map] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

(* Replacement for 'Common.hd_exn "unexpected empty list"', which returns the
   first element of a list or
   fails with an unhelpful exception. 'Common.hd_exn msg []' will raise
   the exception 'Failure msg' which is only a slight improvement over
   'Common.hd_exn "unexpected empty list"'.

   In general, you should prefer a match-with and not have to call a
   function to extract the first element of a list.

   Usage: Common.hd_exn "found an empty list of things" xs

   If receiving an empty list is a bug, prefer the following:

     match xs with
     | [] -> assert false
     | xs -> ...
*)
val hd_exn : string -> 'a list -> 'a

(* Replacement for 'Common.tl_exn "unexpected empty list"' but not a great
   improvement. The same recommendations as for 'Common.hd_exn "unexpected
   empty list"' apply. *)
val tl_exn : string -> 'a list -> 'a list

val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** Same as [List.map2] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(** Same as [List.mapi] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

val flatten : 'a list list -> 'a list
(** Same as [List.flatten] but tail recursive. *)

(* opposite of Common.filter *)
val exclude : ('a -> bool) -> 'a list -> 'a list

(* Sort in a polymorphic way. You should really use 'deriving ord' instead *)
val sort : 'a list -> 'a list
val uniq_by : ('a -> 'a -> bool) -> 'a list -> 'a list

val map_filter : ('a -> 'b option) -> 'a list -> 'b list
(** Same as [List.filter_map] but tail recursive. *)

val find_some : ('a -> 'b option) -> 'a list -> 'b
val find_some_opt : ('a -> 'b option) -> 'a list -> 'b option
val filter_some : 'a option list -> 'a list

(* Haskell-inspired list combinators (take/drop/span) *)

(* this may raise Failure "Common.take: not enough" *)
val take : int -> 'a list -> 'a list

(* this does not raise a Failure and take only the first n elements *)
val take_safe : int -> 'a list -> 'a list

(* this may raise Failure "drop: not enough" *)
val drop : int -> 'a list -> 'a list
val span : ('a -> bool) -> 'a list -> 'a list * 'a list

(* zip a list with an increasing list of numbers.
 * e.g., index_list ["a"; "b"] -> ["a", 0; "b", 1].
 * An alternative is to use functions like List.iteri.
 *)
val index_list : 'a list -> ('a * int) list
val index_list_0 : 'a list -> ('a * int) list

(* similar to index_list_0 but start the index at 1 *)
val index_list_1 : 'a list -> ('a * int) list

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)

type ('a, 'b) assoc = ('a * 'b) list

(* sorts *)
val sort_by_val_lowfirst : ('a, 'b) assoc -> ('a * 'b) list
val sort_by_val_highfirst : ('a, 'b) assoc -> ('a * 'b) list
val sort_by_key_lowfirst : ('a, 'b) assoc -> ('a * 'b) list
val sort_by_key_highfirst : ('a, 'b) assoc -> ('a * 'b) list

(* group by *)
val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list
val group_by_mapped_key : ('a -> 'b) -> 'a list -> ('b * 'a list) list
val group_by_multi : ('a -> 'b list) -> 'a list -> ('b * 'a list) list

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)

type 'a stack = 'a list

val push : 'a -> 'a stack ref -> unit

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

type 'a hashset = ('a, bool) Hashtbl.t

val hashset_of_list : 'a list -> 'a hashset
val hashset_to_list : 'a hashset -> 'a list
val optlist_to_list : 'a list option -> 'a list

(*****************************************************************************)
(* Option *)
(*****************************************************************************)

(* Since OCaml 4.08 you can define your own "binding operator"
 * (see https://v2.ocaml.org/manual/bindingops.html)
 * 'let*' is one such binding operator and an alias to Option.bind.
 * [bind o f] is [f v] if [o] is [Some v] and [None] if [o] is [None].
 *
 * Here is an example of use:
 *  let* x1 = xs |> List.find_opt (fun x -> x > 1) in
 *  let* x2 = xs |> List.find_opt (fun x -> x > 2) in
 *  Some (x1 + x2)
 *
 * Without let*, you would have to write lots of boilerplace code like:
 *  match xs |> List.find_opt (fun x -> x > 1) with
 *  | None -> None
 *  | Some x1 ->
 *    (match xs |> List.find_opt (fun x -> x > 2) in
 *    | None -> None
 *    | Some x2 -> Some (x1 + x2)
 *    )
 *)
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option

(* TODO: we should delete this function; let* is better *)
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( ||| ) : 'a option -> 'a -> 'a

(*****************************************************************************)
(* Either *)
(*****************************************************************************)
(* Note that since OCaml 4.12.0, the standard library has an Either module
 * but it is not recognized by default by ppx_deriving so it's simpler
 * for now to define our own either.
 *)

(* Haskell-inspired either type *)
type ('a, 'b) either = Left of 'a | Right of 'b [@@deriving eq, show, sexp]

val partition_either : ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list

type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
[@@deriving eq, show]

val partition_either3 :
  ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list

(*
   Same as 'partition_either' but operates on the standard type 'result'
   (Ok or Error).
*)
val partition_result :
  ('a -> ('ok, 'error) result) -> 'a list -> 'ok list * 'error list

(*****************************************************************************)
(* Optimizations *)
(*****************************************************************************)

val memoized : ?use_cache:bool -> ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)
(* See also the profiling library and profiling.ppx [@@profiling] annot *)

(*
   Measure how long it takes for a function to run, returning the result
   and the duration.
*)
val with_time : (unit -> 'a) -> 'a * float

(*
   Run a function and print how long it took to return or to raise an
   exception. pr_time prints to stdout, pr2_time prints to stderr.
*)
val pr_time : string -> (unit -> 'a) -> 'a
val pr2_time : string -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* Disable physical equality/inequality operators *)
(*****************************************************************************)

(*
   Disable the use of (==) since some people confuse it with structural
   equality. We do this here since we're disabling in with semgrep anyway
   and it's quicker if the compiler can report it.
*)

(* Physical (shallow) equality, normally available as (==) *)
val phys_equal : 'a -> 'a -> bool

(* Physical (shallow) inequality, normally available as (!=) *)
val phys_not_equal : 'a -> 'a -> bool

type hidden_by_your_nanny

val ( == ) : hidden_by_your_nanny
val ( != ) : hidden_by_your_nanny

val equal_ref_option :
  ('a -> 'b -> bool) -> 'a option ref -> 'b option ref -> bool

(*****************************************************************************)
(* Operators *)
(*****************************************************************************)

(* if you just want to use the operators *)
module Operators : sig
  val ( =~ ) : string -> string -> bool
  val ( = ) : string -> string -> bool
  val ( =|= ) : int -> int -> bool
  val ( =$= ) : char -> char -> bool
  val ( =:= ) : bool -> bool -> bool
  val ( =*= ) : 'a -> 'a -> bool
  val ( == ) : hidden_by_your_nanny
  val ( != ) : hidden_by_your_nanny
end

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* run by main_boilerplate below at its finalize step before exiting.
 * Can be used for example to display some profiling information
 * (see Profiling.ml as an example)
 *)
val before_exit : (unit -> unit) list ref

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit

(* type of maps from string to `a *)
module SMap : Map.S with type key = String.t

type 'a smap = 'a SMap.t

(* you should set this flag when you run code compiled by js_of_ocaml *)
val jsoo : bool ref
