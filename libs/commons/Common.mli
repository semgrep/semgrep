(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module contains functions which are very often used.
 * They are so common (hence the name of this file) that lots of modules
 * just 'open Common' to get in scope those functions.
 * This file acts like a second stdlib.ml (which was called pervasives.ml before)
 *
 * However, because this module is often open'ed, it should
 * not define too many functions (<100) because we can't impose
 * to other programmers the mental effort to know too many functions.
 * This was actually a big problem with the first version of Common.ml
 * (renamed to common2.ml since).
 *)

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

(* You should not use the polymorphic '='. It is convenient but
 * its use will eventually backfire
 * (see https://blog.janestreet.com/the-perils-of-polymorphic-compare/).
 * You should use instead 'deriving eq' where the equality function can be
 * customized. To enforce this rule, this module redefines '=' to just operate
 * on strings, so ocamlc can statically detect when you wrongly use '='
 * on other types.
 *
 * See also the Operators submodule at the end of this file.
 *)

include module type of Eq.Operators

(* Physical (shallow) equality, normally available as (==) *)
val phys_equal : 'a -> 'a -> bool

(* Physical (shallow) inequality, normally available as (!=) *)
val phys_not_equal : 'a -> 'a -> bool

val equal_ref_option :
  ('a -> 'b -> bool) -> 'a option ref -> 'b option ref -> bool

(*****************************************************************************)
(* Comparison *)
(*****************************************************************************)
(* now in Ord.ml *)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

(* see also Fun.id *)

val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val do_nothing : unit -> unit
val const : 'a -> 'b -> 'a
val applyn : int -> ('a -> 'a) -> 'a -> 'a
val on : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c

(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)
(* see also Exception.ml functions as well as Time_limit.Timeout
 * and Memory_limit.ExceededMemoryLimit in the process_limits library.
 *)

exception Todo

(* some people prefer assert false *)
exception Impossible

(* similar to Not_found but to use when something returns too many findings *)
exception Multi_found

(* it's usually far easier to diagnose an error when you know on which
 * file it occured. An [Invalid_argument("index out of bounds")] is not
 * as good as [ErrorOnFile("out of bounds in lines_of_file()", 'foo.c')]
 *)
exception ErrorOnFile of string (* error message *) * Fpath.t

(* If the user use some [exit 0] in his code, then no one can intercept this
 * exit and do something before exiting. There is exn handler for exit 0
 * so better never use exit 0 but instead use an exception and just at
 * the very toplevel transform this exn in a unix exit code.
 *
 * subtil: same problem than with Timeout. Do not intercept such exception
 * with some blind try (...) with _ -> ...
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

(* Java-inspired combinator (DEPRECATED, use protect()) *)
val finalize : (unit -> 'a) -> (unit -> unit) -> 'a
val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
(* Same as 'Fun.protect' but it will not raise 'Finally_raised', if 'finally'
 * raises any exception then that same exception is what 'protect' will raise.
 * This can easily happen in Semgrep due to the asynchronous
 * 'Time_limit.Timeout' exception raised when there is a timeout. Having to
 * deal with 'Finally_raised' just makes things more complicated.
 *)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

(* to be used in pipes as in foo() |> before_return (fun v -> pr2_gen v)*)
val before_return : ('a -> unit) -> 'a -> 'a

(*****************************************************************************)
(* Strings and regexps *)
(*****************************************************************************)

(* shortcuts for string_of_int and int_of_string *)
val i_to_s : int -> string
val s_to_i : string -> int

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

(*****************************************************************************)
(* Lists *)
(*****************************************************************************)
(* now in List_.mli *)

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
(* now in Assoc.mli *)

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)
(* now in Stack_.mli *)

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)
(* now in Hashtbl_.mli *)

(*****************************************************************************)
(* Polymorphic Set and Map *)
(*****************************************************************************)
(* now in Set_.mli and Map_.mli *)

(*****************************************************************************)
(* Polymorphic String Map *)
(*****************************************************************************)

(* type of maps from string to `a *)
module SMap : Map.S with type key = String.t

type 'a smap = 'a SMap.t

(*****************************************************************************)
(* Option *)
(*****************************************************************************)
(* See also List_.mli with many option related functions (e.g., map_filter) *)

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
val ( ||| ) : 'a option -> 'a -> 'a

(*****************************************************************************)
(* Result *)
(*****************************************************************************)
(* Now in Result_.mli *)
val ( let/ ) :
  ('a, 'e) Result.t -> ('a -> ('b, 'e) Result.t) -> ('b, 'e) Result.t

(*****************************************************************************)
(* Either *)
(*****************************************************************************)
(* Now in Either_.mli *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
(* Inputs a line of text in a platform-agnostic way. Should be preferred over
   `input_line`, especially when dealing with Windows.
   More info can be found in `Common.ml`.
   This in-channel should be opened in binary mode.
*)
val input_text_line : in_channel -> string

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
   LATER: could be moved to CapCommon.ml
*)
val with_time : (unit -> 'a) -> 'a * float

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
  val ( == ) : Eq.hidden_by_your_nanny
  val ( != ) : Eq.hidden_by_your_nanny
end

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* you should set this flag when you run code compiled by js_of_ocaml *)
val jsoo : bool ref
