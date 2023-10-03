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

open Sexplib.Std

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

exception UnixExit of int

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

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

let pr s =
  print_string s;
  print_string "\n";
  flush stdout

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
(* Composition/Control *)
(*****************************************************************************)

let protect ~finally work =
  let prev_blocked = ref None in
  let finally () =
    (try
       prev_blocked := Some (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigalrm ])
     with
    | Invalid_argument _ -> (* TODO: on Windows ? *) ());
    finally ()
    (* If we unblock here, we could get a Timeout exception inside 'finally' ... *)
  in
  (* nosemgrep: no-fun-protect *)
  let x = Fun.protect ~finally work in
  (* If 'prev_blocked' is 'None' then nothing was blocked... if it's 'Some _' then
   * we are not on Windows so future calls should not raise any exception. *)
  !prev_blocked
  |> Option.iter (fun prev_blocked ->
         Unix.sigprocmask Unix.SIG_SETMASK prev_blocked |> ignore);
  x

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)
(* see also profiling/Profiling.ml now *)

(* Report the time a function takes. *)
let with_time f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  (res, t2 -. t1)

let pr_time name f =
  let t1 = Unix.gettimeofday () in
  protect f ~finally:(fun () ->
      let t2 = Unix.gettimeofday () in
      pr (spf "%s: %.6f s" name (t2 -. t1)))

let pr2_time name f =
  let t1 = Unix.gettimeofday () in
  protect f ~finally:(fun () ->
      let t2 = Unix.gettimeofday () in
      pr2 (spf "%s: %.6f s" name (t2 -. t1)))

(*###########################################################################*)
(* List functions *)
(*###########################################################################*)

(*****************************************************************************)
(* List.map *)
(*****************************************************************************)

(*
   Custom list type used to store intermediate lists, while minimizing
   the number of allocated blocks.
*)
type 'a list5 =
  | Elt of 'a * 'a list5
  | Tuple of 'a * 'a * 'a * 'a * 'a * 'a list5
  | Empty

let rev5 l =
  let rec aux acc l =
    match l with
    | Tuple (e, d, c, b, a, l) ->
        (* common case *)
        aux (a :: b :: c :: d :: e :: acc) l
    | Elt (a, l) -> aux (a :: acc) l
    | Empty -> acc
  in
  aux [] l

let rec slow_map acc f l =
  match l with
  | [] -> rev5 acc
  | [ a ] -> rev5 (Elt (f a, acc))
  | [ a; b ] ->
      let a = f a in
      let b = f b in
      rev5 (Elt (b, Elt (a, acc)))
  | [ a; b; c ] ->
      let a = f a in
      let b = f b in
      let c = f c in
      rev5 (Elt (c, Elt (b, Elt (a, acc))))
  | [ a; b; c; d ] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      rev5 (Elt (d, Elt (c, Elt (b, Elt (a, acc)))))
  | [ a; b; c; d; e ] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      rev5 (Elt (e, Elt (d, Elt (c, Elt (b, Elt (a, acc))))))
  | a :: b :: c :: d :: e :: l ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      slow_map (Tuple (e, d, c, b, a, acc)) f l

let rec fast_map rec_calls_remaining f l =
  if rec_calls_remaining <= 0 then slow_map Empty f l
  else
    match l with
    | [] -> []
    | [ a ] -> [ f a ]
    | [ a; b ] ->
        let a = f a in
        let b = f b in
        [ a; b ]
    | [ a; b; c ] ->
        let a = f a in
        let b = f b in
        let c = f c in
        [ a; b; c ]
    | [ a; b; c; d ] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        [ a; b; c; d ]
    | [ a; b; c; d; e ] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        [ a; b; c; d; e ]
    | a :: b :: c :: d :: e :: l ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        a :: b :: c :: d :: e :: fast_map (rec_calls_remaining - 1) f l

(*
   This implementation of List.map makes at most 1000 non-tailrec calls
   before switching to a slower tailrec implementation.

   Additionally, this implementation guarantees left-to-right evaluation.
*)
let map f l = fast_map 1000 f l

(*****************************************************************************)
(* List.map2 *)
(*****************************************************************************)

let rec slow_map2 acc f l1 l2 =
  match (l1, l2) with
  | [], [] -> rev5 acc
  | [ a1 ], [ a2 ] -> rev5 (Elt (f a1 a2, acc))
  | [ a1; b1 ], [ a2; b2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      rev5 (Elt (b, Elt (a, acc)))
  | [ a1; b1; c1 ], [ a2; b2; c2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      rev5 (Elt (c, Elt (b, Elt (a, acc))))
  | [ a1; b1; c1; d1 ], [ a2; b2; c2; d2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      let d = f d1 d2 in
      rev5 (Elt (d, Elt (c, Elt (b, Elt (a, acc)))))
  | [ a1; b1; c1; d1; e1 ], [ a2; b2; c2; d2; e2 ] ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      let d = f d1 d2 in
      let e = f e1 e2 in
      rev5 (Elt (e, Elt (d, Elt (c, Elt (b, Elt (a, acc))))))
  | a1 :: b1 :: c1 :: d1 :: e1 :: l1, a2 :: b2 :: c2 :: d2 :: e2 :: l2 ->
      let a = f a1 a2 in
      let b = f b1 b2 in
      let c = f c1 c2 in
      let d = f d1 d2 in
      let e = f e1 e2 in
      slow_map2 (Tuple (e, d, c, b, a, acc)) f l1 l2
  | _other -> raise (Failure "Common.map2: lists not equal length")

let rec fast_map2 rec_calls_remaining f l1 l2 =
  if rec_calls_remaining <= 0 then slow_map2 Empty f l1 l2
  else
    match (l1, l2) with
    | [], [] -> []
    | [ a1 ], [ a2 ] -> [ f a1 a2 ]
    | [ a1; b1 ], [ a2; b2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        [ a; b ]
    | [ a1; b1; c1 ], [ a2; b2; c2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        [ a; b; c ]
    | [ a1; b1; c1; d1 ], [ a2; b2; c2; d2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        let d = f d1 d2 in
        [ a; b; c; d ]
    | [ a1; b1; c1; d1; e1 ], [ a2; b2; c2; d2; e2 ] ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        let d = f d1 d2 in
        let e = f e1 e2 in
        [ a; b; c; d; e ]
    | a1 :: b1 :: c1 :: d1 :: e1 :: l1, a2 :: b2 :: c2 :: d2 :: e2 :: l2 ->
        let a = f a1 a2 in
        let b = f b1 b2 in
        let c = f c1 c2 in
        let d = f d1 d2 in
        let e = f e1 e2 in
        a :: b :: c :: d :: e :: fast_map2 (rec_calls_remaining - 1) f l1 l2
    | _other -> raise (Failure "Common.map2: lists not equal length")

(*
   This implementation of List.map makes at most 1000 non-tailrec calls
   before switching to a slower tailrec implementation.

   Additionally, this implementation guarantees left-to-right evaluation.
*)
let map2 f l1 l2 = fast_map2 1000 f l1 l2

(*****************************************************************************)
(* Other list functions *)
(*****************************************************************************)

let hd_exn errmsg xs =
  match xs with
  | [] -> failwith errmsg
  | head :: _ -> head

let tl_exn errmsg xs =
  match xs with
  | [] -> failwith errmsg
  | _ :: tail -> tail

let mapi f l = map2 f (List.init (List.length l) Fun.id) l

(* Tail-recursive to prevent stack overflows. *)
let flatten xss =
  xss |> List.fold_left (fun acc xs -> List.rev_append xs acc) [] |> List.rev

let rec drop n xs =
  match (n, xs) with
  | 0, _ -> xs
  | _, [] -> failwith "drop: not enough"
  | n, _x :: xs -> drop (n - 1) xs

let take n xs =
  let rec next n xs acc =
    match (n, xs) with
    | 0, _ -> List.rev acc
    | _, [] -> failwith "Common.take: not enough"
    | n, x :: xs -> next (n - 1) xs (x :: acc)
  in
  next n xs []

let enum x n =
  if not (x <= n) then
    failwith (Printf.sprintf "bad values in enum, expect %d <= %d" x n);
  let rec enum_aux acc x n =
    if x =|= n then n :: acc else enum_aux (x :: acc) (x + 1) n
  in
  List.rev (enum_aux [] x n)

let exclude p xs = List.filter (fun x -> not (p x)) xs

let rec (span : ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p -> function
  | [] -> ([], [])
  | x :: xs ->
      if p x then
        let l1, l2 = span p xs in
        (x :: l1, l2)
      else ([], x :: xs)

let rec take_safe n xs =
  match (n, xs) with
  | 0, _ -> []
  | _, [] -> []
  | n, x :: xs -> x :: take_safe (n - 1) xs

(* Partition elements by key. Preserve the original order. *)
let group_by f xs =
  (* use Hashtbl.find_all property *)
  let h = Hashtbl.create 101 in

  (* could use Set *)
  let hkeys = Hashtbl.create 101 in

  xs
  |> List.iter (fun x ->
         let k = f x in
         Hashtbl.replace hkeys k true;
         Hashtbl.add h k x);
  Hashtbl.fold
    (fun k _ acc -> (k, Hashtbl.find_all h k |> List.rev) :: acc)
    hkeys []

let group_by_multi fkeys xs =
  (* use Hashtbl.find_all property *)
  let h = Hashtbl.create 101 in

  (* could use Set *)
  let hkeys = Hashtbl.create 101 in

  xs
  |> List.iter (fun x ->
         let ks = fkeys x in
         ks
         |> List.iter (fun k ->
                Hashtbl.replace hkeys k true;
                Hashtbl.add h k x));
  Hashtbl.fold (fun k _ acc -> (k, Hashtbl.find_all h k) :: acc) hkeys []

(* you should really use group_assoc_bykey_eff *)
let rec group_by_mapped_key fkey l =
  match l with
  | [] -> []
  | x :: xs ->
      let k = fkey x in
      let xs1, xs2 =
        List.partition
          (fun x' ->
            let k2 = fkey x' in
            k =*= k2)
          xs
      in
      (k, x :: xs1) :: group_by_mapped_key fkey xs2

let rec zip xs ys =
  match (xs, ys) with
  | [], [] -> []
  | [], _ -> failwith "zip: not same length"
  | _, [] -> failwith "zip: not same length"
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let null xs =
  match xs with
  | [] -> true
  | _ -> false

let index_list xs =
  if null xs then [] (* enum 0 (-1) generate an exception *)
  else zip xs (enum 0 (List.length xs - 1))

let index_list_0 xs = index_list xs
let index_list_1 xs = xs |> index_list |> map (fun (x, i) -> (x, i + 1))
let sort_prof a b = List.sort a b

let sort_by_val_highfirst xs =
  sort_prof (fun (_k1, v1) (_k2, v2) -> compare v2 v1) xs

let sort_by_val_lowfirst xs =
  sort_prof (fun (_k1, v1) (_k2, v2) -> compare v1 v2) xs

let sort_by_key_highfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k2 k1) xs

let sort_by_key_lowfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k1 k2) xs

let push v l = l := v :: !l

(*###########################################################################*)
(* Exn *)
(*###########################################################################*)

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

let exn_to_s exn = Printexc.to_string exn

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Test *)
(*****************************************************************************)
(* See Testutil *)

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
(* Option/Either *)
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

(* often used in grammar actions in menhir *)
let optlist_to_list = function
  | None -> []
  | Some xs -> xs

(* not sure why but can't use let (?:) a b = ... then at use time ocaml yells*)
let ( ||| ) a b =
  match a with
  | Some x -> x
  | None -> b

type ('a, 'b) either = Left of 'a | Right of 'b [@@deriving eq, show, sexp]

(* with sexp *)
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
[@@deriving eq, show]
(* with sexp *)

(* If you don't want to use [@@deriving eq, show] above, you
 * can copy-paste manually the generated code by getting the
 * result of ocamlfind ocamlc -dsource ... on this code
 *  type ('a, 'b) either =
 *  | Left of 'a
 *  | Right of 'b
 *  [@@deriving show]
 *
 * which should look like this:
 * let pp_either = fun poly_a -> fun poly_b -> fun fmt -> function
 *   | Left a0 ->
 *       (Format.fprintf fmt "(@[<2>Left@ ";
 *        (poly_a fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Right a0 ->
 *       (Format.fprintf fmt "(@[<2>Right@ ";
 *        (poly_b fmt) a0;
 *        Format.fprintf fmt "@])")
 *
 * let pp_either3 = fun poly_a -> fun poly_b -> fun poly_c -> fun fmt -> function
 *   | Left3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Left3@ ";
 *        (poly_a fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Middle3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Middle3@ ";
 *        (poly_b fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Right3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Right3@ ";
 *        (poly_c fmt) a0;
 *        Format.fprintf fmt "@])")
 *)

let partition_either f l =
  let rec part_either left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match f x with
        | Left e -> part_either (e :: left) right l
        | Right e -> part_either left (e :: right) l)
  in
  part_either [] [] l

let partition_either3 f l =
  let rec part_either left middle right = function
    | [] -> (List.rev left, List.rev middle, List.rev right)
    | x :: l -> (
        match f x with
        | Left3 e -> part_either (e :: left) middle right l
        | Middle3 e -> part_either left (e :: middle) right l
        | Right3 e -> part_either left middle (e :: right) l)
  in
  part_either [] [] [] l

let partition_result f l =
  let rec aux left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match f x with
        | Ok x -> aux (x :: left) right l
        | Error x -> aux left (x :: right) l)
  in
  aux [] [] l

let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l

(* Tail-recursive to prevent stack overflows. *)
let map_filter f xs =
  List.fold_left
    (fun acc x ->
      match f x with
      | None -> acc
      | Some y -> y :: acc)
    [] xs
  |> List.rev

let rec find_some_opt p = function
  | [] -> None
  | x :: l -> (
      match p x with
      | Some v -> Some v
      | None -> find_some_opt p l)

let find_some p xs =
  match find_some_opt p xs with
  | None -> raise Not_found
  | Some x -> x

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
(* Filenames *)
(*****************************************************************************)

(* TODO: we should use strong types like in Li Haoyi filename Scala library! *)
type filename = string (* TODO could check that exist :) type sux *)
[@@deriving show, eq, ord, sexp]

let chop_dirsymbol = function
  | s when s =~ "\\(.*\\)/$" -> matched1 s
  | s -> s

(* pre: prj_path must not contain regexp symbol *)
let filename_without_leading_path prj_path s =
  let prj_path = chop_dirsymbol prj_path in
  if s = prj_path then "."
  else if
    (* Note that we should handle multiple consecutive '/' as in 'path/to//file' *)
    s =~ "^" ^ prj_path ^ "/+\\(.*\\)$"
  then matched1 s
  else
    failwith (spf "cant find filename_without_project_path: %s  %s" prj_path s)

(* Deprecated: use the Ppath.ml module instead! *)
let readable ~root s =
  match root with
  | "/" -> s
  | "." -> (
      match s with
      | s when s =~ "^/" ->
          failwith (spf "file %s shouldn't start with / when root is ." s)
      | s when s =~ "^\\./\\(.*\\)" -> matched1 s
      | _ -> s)
  | _ -> filename_without_leading_path root s

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)

exception CmdError of Unix.process_status * string

let process_output_to_list2 ?(verbose = false) command =
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e :: !res;
    if verbose then pr2 e;
    process_otl_aux ()
  in
  try process_otl_aux () with
  | End_of_file ->
      let stat = Unix.close_process_in chan in
      (List.rev !res, stat)

let cmd_to_list ?verbose command =
  let l, exit_status = process_output_to_list2 ?verbose command in
  match exit_status with
  | Unix.WEXITED 0 -> l
  | _ ->
      raise
        (CmdError
           ( exit_status,
             spf "CMD = %s, RESULT = %s" command (String.concat "\n" l) ))

let cmd_to_list_and_status = process_output_to_list2

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

let cat file =
  let acc = ref [] in
  let chan = open_in_bin file in
  try
    while true do
      acc := input_text_line chan :: !acc
    done;
    assert false
  with
  | End_of_file ->
      close_in chan;
      List.rev !acc

(*
   This implementation works even with Linux files like /dev/fd/63
   created by bash's process substitution e.g.

     my-ocaml-program <(echo contents)

   See https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   In bash, '<(echo contents)' is replaced by something like
   '/dev/fd/63' which is a special file of apparent size 0 (as
   reported by `Unix.stat`) but contains data (here,
   "contents\n"). So we can't use 'Unix.stat' or 'in_channel_length'
   to obtain the length of the file contents. Instead, we read the file
   chunk by chunk until there's nothing left to read.

   Why such a function is not provided by the ocaml standard library is
   unclear.
*)
let read_file ?(max_len = max_int) path =
  if !jsoo then (
    let ic = open_in_bin path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s)
  else
    let buf_len = 4096 in
    let extbuf = Buffer.create 4096 in
    let buf = Bytes.create buf_len in
    let rec loop fd =
      match Unix.read fd buf 0 buf_len with
      | 0 -> Buffer.contents extbuf
      | num_bytes ->
          assert (num_bytes > 0);
          assert (num_bytes <= buf_len);
          Buffer.add_subbytes extbuf buf 0 num_bytes;
          if Buffer.length extbuf >= max_len then Buffer.sub extbuf 0 max_len
          else loop fd
    in
    let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
    protect ~finally:(fun () -> Unix.close fd) (fun () -> loop fd)

let write_file ~file s =
  let chan = open_out_bin file in
  output_string chan s;
  close_out chan

(* could be in control section too *)

let fullpath file =
  if not (Sys.file_exists file) then
    failwith (spf "fullpath: file (or directory) %s does not exist" file);
  let dir, base =
    if Sys.is_directory file then (file, None)
    else (Filename.dirname file, Some (Filename.basename file))
  in
  (* save *)
  let old = Sys.getcwd () in

  Sys.chdir dir;
  let here = Sys.getcwd () in

  (* restore *)
  Sys.chdir old;

  match base with
  | None -> here
  | Some x -> Filename.concat here x

(* emacs/lisp inspiration (eric cooper and yaron minsky use that too) *)
let (with_open_outfile :
      filename -> ((string -> unit) * out_channel -> 'a) -> 'a) =
 fun file f ->
  let chan = open_out_bin file in
  let pr s = output_string chan s in
  unwind_protect
    (fun () ->
      let res = f (pr, chan) in
      close_out chan;
      res)
    (fun _e -> close_out chan)

let (with_open_infile : filename -> (in_channel -> 'a) -> 'a) =
 fun file f ->
  let chan = open_in_bin file in
  unwind_protect
    (fun () ->
      let res = f chan in
      close_in chan;
      res)
    (fun _e -> close_in chan)

(* creation of tmp files, a la gcc *)

let _temp_files_created = Hashtbl.create 101

(* ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" *)
let new_temp_file prefix suffix =
  let pid = if !jsoo then 42 else Unix.getpid () in
  let processid = i_to_s pid in
  let tmp_file = Filename.temp_file (prefix ^ "-" ^ processid ^ "-") suffix in
  Hashtbl.add _temp_files_created tmp_file ();
  tmp_file

let save_tmp_files = ref false

let erase_temp_files () =
  if not !save_tmp_files then (
    _temp_files_created
    |> Hashtbl.iter (fun s () ->
           logger#info "erasing: %s" s;
           Sys.remove s);
    Hashtbl.clear _temp_files_created)

let erase_this_temp_file f =
  if not !save_tmp_files then (
    Hashtbl.remove _temp_files_created f;
    logger#info "erasing: %s" f;
    Sys.remove f)

(*###########################################################################*)
(* Collection-like types other than List *)
(*###########################################################################*)

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
type ('a, 'b) assoc = ('a * 'b) list

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

(*****************************************************************************)
(* Matrix *)
(*****************************************************************************)

(*****************************************************************************)
(* Set. Have a look too at set*.mli  *)
(*****************************************************************************)

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] |> List.sort compare

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t
(* with sexp *)

let hashset_to_list h = hash_to_list h |> map fst

(* old: slightly slower?
 * let hashset_of_list xs =
 *   xs +> map (fun x -> x, true) +> hash_of_list
 *)
let hashset_of_list (xs : 'a list) : ('a, bool) Hashtbl.t =
  let h = Hashtbl.create (List.length xs) in
  xs |> List.iter (fun k -> Hashtbl.replace h k true);
  h

let hkeys h =
  let hkey = Hashtbl.create 101 in
  h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
  hashset_to_list hkey

let group_assoc_bykey_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.add h k v);
  let keys = hkeys h in
  keys |> map (fun k -> (k, Hashtbl.find_all h k))

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)

type 'a stack = 'a list
(* with sexp *)

(*****************************************************************************)
(* Tree *)
(*****************************************************************************)

(*****************************************************************************)
(* Graph. Have a look too at Ograph_*.mli  *)
(*****************************************************************************)

(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)
let sort xs = List.sort compare xs

(* maybe too slow? use an hash instead to first group, and then in
 * that group remove duplicates? *)
let rec uniq_by eq xs =
  match xs with
  | [] -> []
  | x :: xs -> (
      match List.find_opt (fun y -> eq x y) xs with
      | Some _ -> uniq_by eq xs
      | None -> x :: uniq_by eq xs)

(*###########################################################################*)
(* Misc functions *)
(*###########################################################################*)

(*###########################################################################*)
(* Postlude *)
(*###########################################################################*)

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)

(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* now in prelude: exception UnixExit of int *)
let exn_to_real_unixexit f =
  try f () with
  | UnixExit x -> exit x

let pp_do_in_zero_box f =
  Format.open_box 0;
  f ();
  Format.close_box ()

let before_exit = ref []

let main_boilerplate f =
  if not !Sys.interactive then
    exn_to_real_unixexit (fun () ->
        Sys.set_signal Sys.sigint
          (Sys.Signal_handle
             (fun _ ->
               pr2 "C-c intercepted, will do some cleaning before exiting";
               (* But if do some try ... with e -> and if do not reraise the exn,
                * the bubble never goes at top and so I cant really C-c.
                *
                * A solution would be to not raise, but do the erase_temp_file in the
                * syshandler, here, and then exit.
                * The current solution is to not do some wild  try ... with e
                * by having in the exn handler a case: UnixExit x -> raise ... | e ->
                *)
               Sys.set_signal Sys.sigint Sys.Signal_default;
               raise (UnixExit (-1))));

        (* The finalize() below makes it tedious to go back from exns when we use
         * 'back' in ocamldebug. Hence the special code in finalize() to
         * run differently when in "debugger mode". However the
         * Common.debugger global will be set in main(), so too late, so
         * we have to be quicker here and set it for the finalize() below.
         *)
        if
          Sys.argv |> Array.to_list
          |> List.exists (fun x -> x = "-debugger" || x = "--debugger")
        then debugger := true;

        finalize
          (fun () ->
            pp_do_in_zero_box (fun () ->
                try f () with
                (* <---- here it is *)
                | Unix.Unix_error (e, fm, argm) ->
                    pr2
                      (spf "exn Unix_error: %s %s %s\n" (Unix.error_message e)
                         fm argm);
                    raise (Unix.Unix_error (e, fm, argm))))
          (fun () ->
            !before_exit |> List.iter (fun f -> f ());
            erase_temp_files ()))
(* let _ = if not !Sys.interactive then (main ()) *)

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

let follow_symlinks = ref false

let vcs_re =
  "(^((\\.hg)|(CVS)|(\\.git)|(_darcs)|(\\.svn))$)|(.*\\.git_annot$)|(.*\\.marshall$)"
  |> Re.Posix.re |> Re.compile

let files_of_dir_or_files_no_vcs_nofilter xs =
  xs
  |> map (fun x ->
         if Sys.is_directory x then
           let files = dir_contents x in
           List.filter (fun x -> not (Re.execp vcs_re x)) files
         else [ x ])
  |> flatten

(*****************************************************************************)
(* Maps *)
(*****************************************************************************)

module SMap = Map.Make (String)

type 'a smap = 'a SMap.t

(*****************************************************************************)
(* Disable physical equality/inequality operators *)
(*****************************************************************************)

let phys_equal = Stdlib.( == )
let phys_not_equal = Stdlib.( != )

type hidden_by_your_nanny = unit

let ( == ) : hidden_by_your_nanny = ()
let ( != ) : hidden_by_your_nanny = ()

(* Used to allow choice of whether id_info fields should be checked *)
let equal_ref_option equal_f a b =
  match (!a, !b) with
  | None, None -> true
  | Some a, Some b -> equal_f a b
  | Some _, None
  | None, Some _ ->
      false

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
