(* Yoann Padioleau
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

let logger = Logging.get_logger [ __MODULE__ ]

(*###########################################################################*)
(* Prelude *)
(*###########################################################################*)

(* if set, certain functions like unwind_protect will not
 * do a try and finalize and instead just call the function, which
 * helps in ocamldebug and also in getting better backtraces.
 * This is also useful to set in a js_of_ocaml (jsoo) context to
 * again get better backtraces.
 *)
let debugger = ref false

(* You should set this to true when you run code compiled by js_of_ocaml
 * so some functions can change their implementation and rely
 * less on non-portable API like Unix which does not work well under
 * node or in the browser.
 *)
let jsoo = ref false

(*****************************************************************************)
(* Circular dependencies *)
(*****************************************************************************)

(* The following functions should be in their respective sections but
 * because some functions in some sections use functions in other
 * sections, and because I don't want to take care of the order of
 * those sections (of those dependencies), I put the functions causing
 * dependency problem here. C is better than OCaml on this with the
 * ability to declare prototypes, enabling some form of forward
 * references.
 *)

let spf = Printf.sprintf

(* alt: We tried using 'Unix.sigprocmask' to temporarily block 'SIGALRM' but
 * somehow, in rare cases (e.g.run p/default on repos/brotli/js/decode.js) we
 * end up calling 'Time_limit.set_timeout' while 'SIGALRM' is *blocked*.
 * Unclear why, are we perhaps calling 'set_timeout' from within a
 * 'finally'? Or is 'Unix.sigprocmask' failing to restore the signal mask?
 * It works if we block/unblock (rather than block/restore) but this does not
 * play well with calls to 'protect' nested inside 'finally' blocks.
 *)
let protect ~finally work =
  (* nosemgrep: no-fun-protect *)
  try Fun.protect ~finally work with
  | Fun.Finally_raised exn1 as exn ->
      (* Just re-raise whatever exception was raised during a 'finally',
       * drop 'Finally_raised'.
       *)
      logger#error "protect: %s" (exn |> Exception.catch |> Exception.to_string);
      Exception.catch_and_reraise exn1

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)
let ( =|= ) : int -> int -> bool = ( = )
let ( =$= ) : char -> char -> bool = ( = )
let ( =:= ) : bool -> bool -> bool = ( = )

(* dangerous, do not use, see the comment in Common.mli *)
let ( =*= ) = ( = )

(* To forbid people to use the polymorphic '='.
 * See https://blog.janestreet.com/the-perils-of-polymorphic-compare/
 *)
let ( = ) = String.equal

(* Used to give choice whether id_info fields should be checked in semgrep *)
let equal_ref_option equal_f a b =
  match (!a, !b) with
  | None, None -> true
  | Some a, Some b -> equal_f a b
  | Some _, None
  | None, Some _ ->
      false

(*****************************************************************************)
(* Disable physical equality/inequality operators *)
(*****************************************************************************)

let phys_equal = ( == )
let phys_not_equal = ( != )

type hidden_by_your_nanny = unit

let ( == ) : hidden_by_your_nanny = ()
let ( != ) : hidden_by_your_nanny = ()

(*****************************************************************************)
(* Comparison *)
(*****************************************************************************)

type order = Less | Equal | Greater

(* We use this to be able to factorize our code for binary search, by
   instantiating our code against different kinds of containers and
   element types.
   In particular, this is an improvement over functorization, because the
   type of Bigarray.Array1.t is actually triply-polymorphic. By making the
   container type itself unspecified, we are able to abstract over even
   multiply-polymorphic containers.
*)
type ('elt, 'container) binary_searchable = {
  length : 'container -> int;
  get : 'container -> int -> 'elt;
}

let create_binary_search (searchable : ('elt, 'container) binary_searchable) =
  let binary_search ~f arr =
    let arr_lo = 0 in
    let arr_hi = searchable.length arr in

    let rec aux lo hi =
      if Int.equal lo hi then Error lo
      else
        let mid = (lo + hi) / 2 in
        match f mid (searchable.get arr mid) with
        | Equal -> Ok (mid, searchable.get arr mid)
        | Less -> aux lo mid
        | Greater -> aux (mid + 1) hi
    in
    aux arr_lo arr_hi
  in
  binary_search

let arr_searchable = { length = Array.length; get = Array.get }

let bigarr1_searchable =
  { length = Bigarray.Array1.dim; get = Bigarray.Array1.get }

let binary_search_arr ~f x = create_binary_search arr_searchable ~f x
let binary_search_bigarr1 ~f x = create_binary_search bigarr1_searchable ~f x

let to_comparison f x y =
  let res = f x y in
  if res < 0 then Less else if res > 0 then Greater else Equal

let cmp target _i x = to_comparison Int.compare target x
let%test _ = binary_search_arr ~f:(cmp 1) [| 1; 2; 4; 5 |] =*= Ok (0, 1)
let%test _ = binary_search_arr ~f:(cmp 2) [| 1; 2; 4; 5 |] =*= Ok (1, 2)
let%test _ = binary_search_arr ~f:(cmp 5) [| 1; 2; 4; 5 |] =*= Ok (3, 5)

(* out of bounds or not in the array returns the position it should be inserted at *)
let%test _ = binary_search_arr ~f:(cmp 6) [| 1; 2; 4; 5 |] =*= Error 4
let%test _ = binary_search_arr ~f:(cmp 3) [| 1; 2; 4; 5 |] =*= Error 2
let%test _ = binary_search_arr ~f:(cmp 0) [| 1; 2; 4; 5 |] =*= Error 0

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

let pr2 s =
  prerr_string s;
  prerr_string "\n";
  flush stderr

let _already_printed = Hashtbl.create 101
let disable_pr2_once = ref false

let xxx_once f s =
  if !disable_pr2_once then pr2 s
  else if not (Hashtbl.mem _already_printed s) then (
    Hashtbl.add _already_printed s true;
    f ("(ONCE) " ^ s))

let pr2_once s = xxx_once pr2 s
let pr2_gen x = pr2 (Dumper.dump x)

(* to be used in pipe operations *)
let before_return f v =
  f v;
  v

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)
(* see also profiling/Profiling.ml now *)

(* Report the time a function takes. *)
let with_time f =
  let t1 = UUnix.gettimeofday () in
  let res = f () in
  let t2 = UUnix.gettimeofday () in
  (res, t2 -. t1)

let pr2_time name f =
  let t1 = UUnix.gettimeofday () in
  protect f ~finally:(fun () ->
      let t2 = UUnix.gettimeofday () in
      pr2 (spf "%s: %.6f s" name (t2 -. t1)))

(*****************************************************************************)
(* Exn *)
(*****************************************************************************)

(* See also Common.protect earlier *)

(* use Common.protect instead? *)
let unwind_protect f cleanup =
  if !debugger then f ()
  else
    try f () with
    | exn ->
        let e = Exception.catch exn in
        cleanup e;
        Exception.reraise e

(* TODO: remove and use 'protect' instead? but then it will not have the
 * !debugger goodies *)
let finalize f cleanup =
  (* Does this debugger mode changes the semantic of the program too much?
   * Is it ok in a debugging context to not call certain cleanup()
   * and let the exn bubble up?
   * TODO: maybe I should not use save_excursion/finalize so much? maybe
   *  -debugger can help to see code that I should refactor?
   *)
  if !debugger then (
    let res = f () in
    cleanup ();
    res)
  else protect f ~finally:cleanup

let save_excursion reference newv f =
  let old = !reference in
  reference := newv;
  finalize f (fun _ -> reference := old)

let memoized ?(use_cache = true) h k f =
  if not use_cache then f ()
  else
    try Hashtbl.find h k with
    | Not_found ->
        let v = f () in
        Hashtbl.add h k v;
        v

exception Todo
exception Impossible
exception Multi_found (* to be consistent with Not_found *)
exception UnixExit of int

let exn_to_s exn = Printexc.to_string exn

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Test *)
(*****************************************************************************)
(* See Alcotest_ext. and Testutil_* modules *)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(*****************************************************************************)
(* Arguments/options and command line *)
(*****************************************************************************)
(* use Cmdliner, or Arg_helpers if you really have to *)

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)
(* See Cmdliner now *)

(*###########################################################################*)
(* Basic types *)
(*###########################################################################*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

(*****************************************************************************)
(* Option *)
(*****************************************************************************)

let ( let* ) = Option.bind

(* type 'a maybe  = Just of 'a | None *)
let ( >>= ) m1 m2 =
  match m1 with
  | None -> None
  | Some x -> m2 x

(*
 (*http://roscidus.com/blog/blog/2013/10/13/ocaml-tips/#handling-option-types*)
  let (|?) maybe default =
    match maybe with
    | Some v -> v
    | None -> Lazy.force default
*)
(* deprecated: use Option.map
   let map_opt f = function
   | None -> None
   | Some x -> Some (f x)
*)
(* deprecated: use Option.iter
   let do_option f = function
   | None -> ()
   | Some x -> f x
*)
(* deprecated: use Option.to_list
   let opt_to_list : 'a option -> 'a list = function
   | None -> []
   | Some x -> [x]
*)

(* not sure why but can't use let (?:) a b = ... then at use time ocaml yells*)
let ( ||| ) a b =
  match a with
  | Some x -> x
  | None -> b

(*****************************************************************************)
(* Either *)
(*****************************************************************************)
(* now in Either_.ml *)

(*****************************************************************************)
(* Regexp, can also use PCRE *)
(*****************************************************************************)

let (matched : int -> string -> string) = fun i s -> Str.matched_group i s
let matched1 s = matched 1 s
let matched2 s = (matched 1 s, matched 2 s)
let matched3 s = (matched 1 s, matched 2 s, matched 3 s)
let matched4 s = (matched 1 s, matched 2 s, matched 3 s, matched 4 s)

let matched5 s =
  (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)

let matched6 s =
  (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)

let matched7 s =
  ( matched 1 s,
    matched 2 s,
    matched 3 s,
    matched 4 s,
    matched 5 s,
    matched 6 s,
    matched 7 s )

let _memo_compiled_regexp = Hashtbl.create 101

let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
  in
  Str.string_match compile_re s 0

let match_func s re = candidate_match_func s re
let ( =~ ) s re = match_func s re
let split sep s = Str.split (Str.regexp sep) s
let join sep xs = String.concat sep xs

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

(* ruby *)
let i_to_s = string_of_int
let s_to_i = int_of_string
let null_string s = s = ""

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with
  | Not_found -> false

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

(*
   Input a line of text from a file in a way that works for Windows files
   or Unix files in any mode on any platform. Line terminators are eaten
   up.

   Known bug: CR characters preceding a CRLF sequence are eaten up anyway
   in text mode on Windows. Workaround: open the file in binary mode.

   Unix file on Unix:    "a\nb"   -> "a"
   Windows file on Unix: "a\r\nb" -> "a"  (* input_line returns "a\r" *)
   Unix file on Windows text mode:      "a\nb" -> "a"
   Unix file on Windows binary mode:    "a\nb" -> "a"
   Windows file on Windows text mode:   "a\r\nb" -> "a"
   Windows file on Windows binary mode: "a\r\nb" -> "a"

   What you need to know about Windows vs. Unix line endings
   =========================================================

   CR designates the byte '\r', LF designates '\n'.
   CRLF designates the sequence "\r\n".

   On Windows, there are two modes for opening a file for reading or writing:
   - binary mode, which is the same as on Unix.
   - text mode, which is special.

   A file opened in text mode on Windows causes the reads and the writes
   to translate line endings:
   - read from text file: CRLF -> LF
   - write to text file: LF -> CRLF

   To avoid these translations when running on Windows, the file must be
   opened in binary mode.

   Q: When must a file be opened in text mode?
   A: Never. Only stdin, stdout, and stderr connected to a console should
      be in text mode but the OS takes care of this for us.

   Q: Really never?
   A: Never for reading. For writing, maybe some Windows applications behave
      so badly that they can't recognize single LFs, in which case local logs
      and such might have to be opened in text mode. See next question.

   Q: Do all text processing applications on Windows support LF line endings
      in input files?
   A: Hopefully. They really should. The only way they don't support single LFs
      is if they open files in binary mode and then search for CRLF
      sequences as line terminators exclusively, which would be strange.

   Q: Do the parsers in pfff support all line endings?
   A: Yes unless there's a bug. All parsers for real programming languages
      should use the pattern '\r'?'\n' to match line endings.
*)
let input_text_line ic =
  let s = input_line ic in
  let len = String.length s in
  if len > 0 && s.[len - 1] =$= '\r' then String.sub s 0 (len - 1) else s

(*###########################################################################*)
(* Containers *)
(*###########################################################################*)

(*****************************************************************************)
(* List *)
(*****************************************************************************)
(* Now in List_.ml *)

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
(* Now in Assoc.ml *)

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

(*****************************************************************************)
(* Matrix *)
(*****************************************************************************)

(*****************************************************************************)
(* Set *)
(*****************************************************************************)
(* Now in Set_.ml *)

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)
(* Now in Hashtbl_.ml *)

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)
(* Now in Stack_.ml *)

(*****************************************************************************)
(* Tree *)
(*****************************************************************************)

(*****************************************************************************)
(* Maps *)
(*****************************************************************************)

module SMap = Map.Make (String)

type 'a smap = 'a SMap.t

(*****************************************************************************)
(* Graph *)
(*****************************************************************************)
(* Now in Ograph_*.ml *)

(*###########################################################################*)
(* Postlude *)
(*###########################################################################*)

(*****************************************************************************)
(* Operators *)
(*****************************************************************************)

module Operators = struct
  let ( =~ ) = ( =~ )
  let ( = ) = ( = ) (* already shadowed *)
  let ( =|= ) = ( =|= )
  let ( =$= ) = ( =$= )
  let ( =:= ) = ( =:= )
  let ( =*= ) = ( =*= )
  let ( == ) = ( == ) (* already shadowed *)
  let ( != ) = ( != ) (* already shadowed *)
end
