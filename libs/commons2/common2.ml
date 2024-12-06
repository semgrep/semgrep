(* Yoann Padioleau
 *
 * Copyright (C) 1998-2009 Yoann Padioleau
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
open Common

(*###########################################################################*)
(* Prelude *)
(*###########################################################################*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The following functions should be in their respective sections but
 * because some functions in some sections use functions in other
 * sections, and because I don't want to take care of the order of
 * those sections, of those dependencies, I put the functions causing
 * dependency problem here. C is better than caml on this with the
 * ability to declare prototype, enabling some form of forward
 * reference.
 *)

let rec (do_n : int -> (unit -> unit) -> unit) =
 fun i f ->
  if i =|= 0 then ()
  else (
    f ();
    do_n (i - 1) f)

let rec (foldn : ('a -> int -> 'a) -> 'a -> int -> 'a) =
 fun f acc i -> if i =|= 0 then acc else foldn f (f acc i) (i - 1)

let sum_int = List.fold_left ( + ) 0

(* could really call it 'for' :) *)
let fold_left_with_index f acc =
  let rec fold_lwi_aux acc n = function
    | [] -> acc
    | x :: xs -> fold_lwi_aux (f acc x n) (n + 1) xs
  in
  fold_lwi_aux acc 0

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let rec drop n xs =
  match (n, xs) with
  | 0, _ -> xs
  | _, [] -> failwith "drop: not enough"
  | n, _x :: xs -> drop (n - 1) xs

let enum_safe x n = if x > n then [] else List_.enum x n

let rec take n xs =
  match (n, xs) with
  | 0, _ -> []
  | _, [] -> failwith "Common.take: not enough"
  | n, x :: xs -> x :: take (n - 1) xs

let exclude p xs = List.filter (fun x -> not (p x)) xs
let last_n n l = List.rev (take n (List.rev l))

(*let last l = List_.hd_exn "unexpected empty list" (last_n 1 l) *)
let rec list_last = function
  | [] -> raise Not_found
  | [ x ] -> x
  | _x :: y :: xs -> list_last (y :: xs)

let (list_of_string : string -> char list) = function
  | "" -> []
  | s -> List_.enum 0 (String.length s - 1) |> List_.map (String.get s)

let (lines : string -> string list) =
 fun s ->
  let rec lines_aux = function
    | [] -> []
    | [ x ] -> if x = "" then [] else [ x ]
    | x :: xs -> x :: lines_aux xs
  in
  Str.split_delim (Str.regexp "\r\n\\|\n") s |> lines_aux

let (matched : int -> string -> string) = fun i s -> Str.matched_group i s

let (with_open_stringbuf : ((string -> unit) * Buffer.t -> unit) -> string) =
 fun f ->
  let buf = Buffer.create 1000 in
  let pr s = Buffer.add_string buf (s ^ "\n") in
  f (pr, buf);
  Buffer.contents buf

let foldl1 p xs =
  match xs with
  | x :: xs -> List.fold_left p x xs
  | [] -> failwith "foldl1: empty list"

let repeat e n =
  let rec repeat_aux acc = function
    | 0 -> acc
    | n when n < 0 -> failwith "repeat"
    | n -> repeat_aux (e :: acc) (n - 1)
  in
  repeat_aux [] n

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

(* I used this in coccinelle where the huge logging of stuff ask for
 * a more organized solution that use more visual indentation hints.
 *
 * todo? could maybe use log4j instead ? or use Format module more
 * consistently ?
 *)

let _tab_level_print = ref 0
let _tab_indent = 5
let _prefix_pr = ref ""

let indent_do f =
  _tab_level_print := !_tab_level_print + _tab_indent;
  Common.finalize f (fun () ->
      _tab_level_print := !_tab_level_print - _tab_indent)

let pr s =
  UStdlib.print_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.print_string " ");
  UStdlib.print_string s;
  UStdlib.print_string "\n";
  flush UStdlib.stdout

let pr_no_nl s =
  UStdlib.print_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.print_string " ");
  UStdlib.print_string s;
  flush UStdlib.stdout

let _chan_pr2 = ref (None : out_channel option)

let out_chan_pr2 ?(newline = true) s =
  match !_chan_pr2 with
  | None -> ()
  | Some chan ->
      output_string chan (s ^ if newline then "\n" else "");
      flush chan

let pr2 s =
  UStdlib.prerr_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.prerr_string " ");
  UStdlib.prerr_string s;
  UStdlib.prerr_string "\n";
  flush UStdlib.stderr;
  out_chan_pr2 s;
  ()

let pr2_no_nl s =
  UStdlib.prerr_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.prerr_string " ");
  UStdlib.prerr_string s;
  flush UStdlib.stderr;
  out_chan_pr2 ~newline:false s;
  ()

let pr_xxxxxxxxxxxxxxxxx () =
  pr "-----------------------------------------------------------------------"

let pr2_xxxxxxxxxxxxxxxxx () =
  pr2 "-----------------------------------------------------------------------"

let reset_pr_indent () = _tab_level_print := 0

(* old:
 * let pr s = (print_string s; print_string "\n"; flush stdout)
 * let pr2 s = (prerr_string s; prerr_string "\n"; flush stderr)
 *)

(* ---------------------------------------------------------------------- *)

(* I can not use the _xxx ref tech that I use for common_extra.ml here because
 * ocaml don't like the polymorphism of Dumper mixed with refs.
 *
 * let (_dump_func : ('a -> string) ref) = ref
 * (fun x -> failwith "no dump yet, have you included common_extra.cmo?")
 * let (dump : 'a -> string) = fun x ->
 * !_dump_func x
 *
 * So I have included directly dumper.ml in common.ml. It's more practical
 * when want to give script that use my common.ml, I just have to give
 * this file.
 *)

(* ---------------------------------------------------------------------- *)
let pr2_gen x = pr2 (Dumper.dump x)

(* ---------------------------------------------------------------------- *)
let xxx_once f s =
  match () with
  | _ when !UCommon.disable_pr2_once ->
      (* nosemgrep: no-pr2 *)
      UCommon.pr2 s
  | _ when not (Hashtbl.mem UCommon._already_printed s) ->
      Hashtbl.add UCommon._already_printed s true;
      f ("(ONCE) " ^ s)
  | _else_ -> ()

let pr2_once s = xxx_once pr2 s

(* ---------------------------------------------------------------------- *)
let mk_pr2_wrappers aref =
  let fpr2 s =
    if !aref then pr2 s else (* just to the log file *)
                          out_chan_pr2 s
  in
  let fpr2_once s = if !aref then pr2_once s else xxx_once out_chan_pr2 s in
  (fpr2, fpr2_once)

(* ---------------------------------------------------------------------- *)
(* could also be in File section *)

let redirect_stdout file f =
  let chan = UStdlib.open_out_bin file in
  let descr = Unix.descr_of_out_channel chan in

  let saveout = Unix.dup UUnix.stdout in
  Unix.dup2 descr UUnix.stdout;
  flush UStdlib.stdout;
  let res = f () in
  flush UStdlib.stdout;
  Unix.dup2 saveout UUnix.stdout;
  close_out chan;
  res

let redirect_stdout_opt optfile f =
  match optfile with
  | None -> f ()
  | Some outfile -> redirect_stdout outfile f

let redirect_stdout_stderr file f =
  let chan = UStdlib.open_out_bin file in
  let descr = Unix.descr_of_out_channel chan in

  let saveout = Unix.dup UUnix.stdout in
  let saveerr = Unix.dup UUnix.stderr in
  Unix.dup2 descr UUnix.stdout;
  Unix.dup2 descr UUnix.stderr;
  flush UStdlib.stdout;
  flush UStdlib.stderr;
  f ();
  flush UStdlib.stdout;
  flush UStdlib.stderr;
  Unix.dup2 saveout UUnix.stdout;
  Unix.dup2 saveerr UUnix.stderr;
  close_out chan

let redirect_stdin file f =
  let chan = UStdlib.open_in_bin file in
  let descr = Unix.descr_of_in_channel chan in

  let savein = Unix.dup UUnix.stdin in
  Unix.dup2 descr UUnix.stdin;
  f ();
  Unix.dup2 savein UUnix.stdin;
  close_in chan

let redirect_stdin_opt optfile f =
  match optfile with
  | None -> f ()
  | Some infile -> redirect_stdin infile f

(* cf end
   let with_pr2_to_string f =
*)

(* ---------------------------------------------------------------------- *)

(* old: include Printf, include are evil and graph_code_cmt does not like them*)
(* cf common.mli, fprintf, printf, eprintf, sprintf.
 * also what is this ?
 *  val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
 *  val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
 *)

(* ex of printf:
 *  printf "%02d" i
 * for padding
 *)

let spf = Printf.sprintf

(* ---------------------------------------------------------------------- *)

let _chan = ref UStdlib.stderr

let start_log_file () =
  let filename = spf "/tmp/debugml%d:%d" (UUnix.getuid ()) (UUnix.getpid ()) in
  pr2 (spf "now using %s for logging" filename);
  _chan := UStdlib.open_out_bin filename

let dolog s =
  output_string !_chan (s ^ "\n");
  flush !_chan

let verbose_level = ref 1
let log s = if !verbose_level >= 1 then dolog s
let log2 s = if !verbose_level >= 2 then dolog s
let log3 s = if !verbose_level >= 3 then dolog s
let log4 s = if !verbose_level >= 4 then dolog s
let if_log f = if !verbose_level >= 1 then f ()
let if_log2 f = if !verbose_level >= 2 then f ()
let if_log3 f = if !verbose_level >= 3 then f ()
let if_log4 f = if !verbose_level >= 4 then f ()

(* ---------------------------------------------------------------------- *)

let pause () =
  pr2 "pause: type return";
  ignore (UStdlib.read_line ())

(* was used by fix_caml *)
let _trace_var = ref 0
let add_var () = incr _trace_var
let dec_var () = decr _trace_var
let get_var () = !_trace_var

let (print_n : int -> string -> unit) =
 fun i s -> do_n i (fun () -> UStdlib.print_string s)

let (printerr_n : int -> string -> unit) =
 fun i s -> do_n i (fun () -> UStdlib.prerr_string s)

let _debug = ref true
let debugon () = _debug := true
let debugoff () = _debug := false
let debug f = if !_debug then f () else ()

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

(* now near cmd_to_list: let get_mem() = *)

let memory_stat () =
  let stat = Gc.stat () in
  let conv_mo x = x * 4 / 1000000 in
  Printf.sprintf "maximal = %d Mo\n" (conv_mo stat.Gc.top_heap_words)
  ^ Printf.sprintf "current = %d Mo\n" (conv_mo stat.Gc.heap_words)
  ^ Printf.sprintf "lives   = %d Mo\n" (conv_mo stat.Gc.live_words)
(* Printf.printf "fragments = %d Mo\n" (conv_mo stat.Gc.fragments); *)

let timenow () =
  "sys:"
  ^ string_of_float (USys.time ())
  ^ " seconds" ^ ":real:"
  ^
  let tm = UUnix.time () |> Unix.gmtime in
  (tm.tm_min |> string_of_int)
  ^ " min:"
  ^ (tm.tm_sec |> string_of_int)
  ^ ".00 seconds"

let _count1 = ref 0
let _count2 = ref 0
let _count3 = ref 0
let _count4 = ref 0
let _count5 = ref 0
let count1 () = incr _count1
let count2 () = incr _count2
let count3 () = incr _count3
let count4 () = incr _count4
let count5 () = incr _count5

let profile_diagnostic_basic () =
  Printf.sprintf
    "count1 = %d\ncount2 = %d\ncount3 = %d\ncount4 = %d\ncount5 = %d\n" !_count1
    !_count2 !_count3 !_count4 !_count5

let time_func f =
  (*   let _ = Timing () in *)
  let x = f () in
  (*   let _ = Timing () in *)
  x

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

(* See also OUnit *)

(* commented because does not play well with js_of_ocaml
*)
let example b =
  if b then () else failwith ("ASSERT FAILURE: " ^ Printexc.get_backtrace ())

let _ex1 = assert (List_.enum 1 4 =*= [ 1; 2; 3; 4 ])

let assert_equal a b =
  if not (a =*= b) then
    failwith
      ("assert_equal: those 2 values are not equal:\n\t" ^ Dumper.dump a
     ^ "\n\t" ^ Dumper.dump b ^ "\n")

let (example2 : string -> bool -> unit) =
 fun s b ->
  try assert b with
  | _x -> failwith s

(*-------------------------------------------------------------------*)
let _list_bool = ref []

let (example3 : string -> bool -> unit) =
 fun s b -> _list_bool := (s, b) :: !_list_bool

(* could introduce a fun () otherwise the calculus is made at compile time
 * and this can be long. This would require to redefine test_all.
 *   let (example3: string -> (unit -> bool) -> unit) = fun s func ->
 *   _list_bool := (s,func):: (!_list_bool)
 *
 * I would like to do as a func that take 2 terms, and make an = over it
 * avoid to add this ugly fun (), but pb of type, cant do that :(
 *)

let (test_all : unit -> unit) =
 fun () ->
  List.iter
    (fun (s, b) ->
      UPrintf.printf "%s: %s\n" s (if b then "passed" else "failed"))
    !_list_bool

let ( ++ ) a b = a @ b
let _ex = example3 "++" ([ 1; 2 ] @ [ 3; 4; 5 ] =*= [ 1; 2; 3; 4; 5 ])

(*-------------------------------------------------------------------*)
(* Regression testing *)
(*-------------------------------------------------------------------*)

(* cf end of file. It uses too many other common functions so I
 * have put the code at the end of this file.
 *)

(* todo? take code from julien signoles in calendar-2.0.2/tests *)
(*

(* Generic functions used in the tests. *)

val reset : unit -> unit
val nb_ok : unit -> int
val nb_bug : unit -> int
val test : bool -> string -> unit
val test_exn : 'a Lazy.t -> string -> unit


let ok_ref = ref 0
let ok () = incr ok_ref
let nb_ok () = !ok_ref

let bug_ref = ref 0
let bug () = incr bug_ref
let nb_bug () = !bug_ref

let reset () =
  ok_ref := 0;
  bug_ref := 0

let test x s =
  if x then ok () else begin UPrintf.printf "%s\n" s; bug () end;;

let test_exn x s =
  try
    ignore (Lazy.force x);
    UPrintf.printf "%s\n" s;
    bug ()
  with _ ->
    ok ();;
*)

(*****************************************************************************)
(* Quickcheck like (sfl) *)
(*****************************************************************************)

(* related work:
 *  - http://cedeela.fr/quickcheck-for-ocaml.html
 *)

(*---------------------------------------------------------------------------*)
(* generators *)
(*---------------------------------------------------------------------------*)
type 'a gen = unit -> 'a

let (ig : int gen) = fun () -> URandom.int 10

let (lg : 'a gen -> 'a list gen) =
 fun gen () -> foldn (fun acc _i -> gen () :: acc) [] (URandom.int 10)

let (pg : 'a gen -> 'b gen -> ('a * 'b) gen) =
 fun gen1 gen2 () -> (gen1 (), gen2 ())

let polyg = ig
let (ng : string gen) = fun () -> "a" ^ string_of_int (ig ())

let (oneofl : 'a list -> 'a gen) =
 fun xs () -> List.nth xs (URandom.int (List.length xs))
(* let oneofl l = oneof (List.map always l) *)

let (oneof : 'a gen list -> 'a gen) =
 fun xs -> List.nth xs (URandom.int (List.length xs))

let (always : 'a -> 'a gen) = fun e () -> e

let (frequency : (int * 'a gen) list -> 'a gen) =
 fun xs ->
  let sums = sum_int (List_.map fst xs) in
  let i = URandom.int sums in
  let rec freq_aux acc = function
    | (x, g) :: xs -> if i < acc + x then g else freq_aux (acc + x) xs
    | _ -> failwith "frequency"
  in
  freq_aux 0 xs

let frequencyl l = frequency (List_.map (fun (i, e) -> (i, always e)) l)

(*
let b = oneof [always true; always false] ()
let b = frequency [3, always true; 2, always false] ()
*)

(* cant do this:
 *    let rec (lg: ('a gen) -> ('a list) gen) = fun gen -> oneofl [[]; lg gen ()]
 * nor
 *    let rec (lg: ('a gen) -> ('a list) gen) = fun gen -> oneof [always []; lg gen]
 *
 * because caml is not as lazy as haskell :( fix the pb by introducing a size
 * limit. take the bounds/size as parameter. morover this is needed for
 * more complex type.
 *
 * how make a bintreeg ?? we need recursion
 *
 * let rec (bintreeg: ('a gen) -> ('a bintree) gen) = fun gen () ->
 * let rec aux n =
 * if n = 0 then (Leaf (gen ()))
 * else frequencyl [1, Leaf (gen ()); 4, Branch ((aux (n / 2)), aux (n / 2))]
 * ()
 * in aux 20
 *
 *)

(*---------------------------------------------------------------------------*)
(* property *)
(*---------------------------------------------------------------------------*)

(* todo: a test_all_laws, better syntax (done already a little with ig in
 * place of intg. En cas d'erreur, print the arg that not respect
 *
 * todo: with monitoring, as in haskell, laws = laws2, no need for 2 func,
 * but hard i found
 *
 * todo classify, collect, forall
 *)

(* return None when good, and Just the_problematic_case when bad *)
let (laws : string -> ('a -> bool) -> 'a gen -> 'a option) =
 fun _s func gen ->
  let res =
    foldn
      (fun acc _i ->
        let n = gen () in
        (n, func n) :: acc)
      [] 1000
  in
  let res = List.filter (fun (_x, b) -> not b) res in
  if res =*= [] then None
  else Some (fst (List_.hd_exn "unexpected empty list" res))

let rec (statistic_number : 'a list -> (int * 'a) list) = function
  | [] -> []
  | x :: xs ->
      let splitg, splitd = List.partition (fun y -> y =*= x) xs in
      (1 + List.length splitg, x) :: statistic_number splitd

(* in pourcentage *)
let (statistic : 'a list -> (int * 'a) list) =
 fun xs ->
  let stat_num = statistic_number xs in
  let totals = sum_int (List_.map fst stat_num) in
  List_.map (fun (i, v) -> (i * 100 / totals, v)) stat_num

let (laws2 :
      string -> ('a -> bool * 'b) -> 'a gen -> 'a option * (int * 'b) list) =
 fun _s func gen ->
  let res =
    foldn
      (fun acc _i ->
        let n = gen () in
        (n, func n) :: acc)
      [] 1000
  in
  let stat = statistic (List_.map (fun (_x, (_b, v)) -> v) res) in
  let res = List.filter (fun (_x, (b, _v)) -> not b) res in
  if res =*= [] then (None, stat)
  else (Some (fst (List_.hd_exn "unexpected empty list" res)), stat)

(* todo, do with coarbitrary ?? idea is that given a 'a, generate a 'b
 * depending of 'a and gen 'b, that is modify gen 'b, what is important is
 * that each time given the same 'a, we must get the same 'b !!!
 *)

(*
let (fg: ('a gen) -> ('b gen) -> ('a -> 'b) gen) = fun gen1 gen2 () ->
let b = laws "funs" (fun (f,g,h) -> x <= y ==> (max x y  = y)       )(pg ig ig)
 *)

(*
let one_of xs = List.nth xs (URandom.int (List.length xs))
let take_one xs =
  if empty xs then failwith "Take_one: empty list"
  else
    let i = URandom.int (List.length xs) in
    List.nth xs i, filter_index (fun j _ -> i <> j) xs
*)

(*****************************************************************************)
(* Counter *)
(*****************************************************************************)
let _counter = ref 0

let counter () =
  _counter := !_counter + 1;
  !_counter

let _counter2 = ref 0

let counter2 () =
  _counter2 := !_counter2 + 1;
  !_counter2

let _counter3 = ref 0

let counter3 () =
  _counter3 := !_counter3 + 1;
  !_counter3

type timestamp = int

(*****************************************************************************)
(* String_of *)
(*****************************************************************************)
(* To work with the macro system autogenerated string_of and print_ function
   (kind of deriving a la haskell) *)

(* int, bool, char, float, ref ?, string *)

let string_of_string s = "\"" ^ s "\""
let string_of_list f xs = "[" ^ (xs |> List_.map f |> String.concat ";") ^ "]"
let string_of_unit () = "()"

let string_of_array f xs =
  "[|" ^ (xs |> Array.to_list |> List_.map f |> String.concat ";") ^ "|]"

let string_of_option f = function
  | None -> "None "
  | Some x -> "Some " ^ f x

let print_bool x = UStdlib.print_string (if x then "True" else "False")

let print_option pr = function
  | None -> UStdlib.print_string "None"
  | Some x ->
      UStdlib.print_string "Some (";
      pr x;
      UStdlib.print_string ")"

let print_list pr xs =
  UStdlib.print_string "[";
  List.iter
    (fun x ->
      pr x;
      UStdlib.print_string ",")
    xs;
  UStdlib.print_string "]"

(* specialised
   let (string_of_list: char list -> string) =
   List.fold_left (fun acc x -> acc^(Char.escaped x)) ""
*)

let rec print_between between fn = function
  | [] -> ()
  | [ x ] -> fn x
  | x :: xs ->
      fn x;
      between ();
      print_between between fn xs

let adjust_pp_with_indent f =
  UFormat.open_box !_tab_level_print;
  (*Format.force_newline();*)
  f ();
  UFormat.close_box ();
  UFormat.print_newline ()

let adjust_pp_with_indent_and_header s f =
  UFormat.open_box (!_tab_level_print + String.length s);
  do_n !_tab_level_print (fun () -> UFormat.print_string " ");
  UFormat.print_string s;
  f ();
  UFormat.close_box ();
  UFormat.print_newline ()

let pp_do_in_box f =
  UFormat.open_box 1;
  f ();
  UFormat.close_box ()

let pp_do_in_zero_box f =
  UFormat.open_box 0;
  f ();
  UFormat.close_box ()

let pp_f_in_box f =
  UFormat.open_box 1;
  let res = f () in
  UFormat.close_box ();
  res

let pp s = UFormat.print_string s

(*
 * use as
 * let category__str_conv = [
 *   BackGround, "background";
 *   ForeGround, "ForeGround";
 *   ...
 * ]
 *
 * let (category_of_string, str_of_category) =
 * Common.mk_str_func_of_assoc_conv category__str_conv
 *
 *)
let mk_str_func_of_assoc_conv xs =
  let swap (x, y) = (y, x) in

  ( (fun s ->
      let xs' = List_.map swap xs in
      List.assoc s xs'),
    fun a -> List.assoc a xs )

(* julia: convert something printed using format to print into a string *)
(* now at bottom of file
   let format_to_string f =
   ...
*)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

(* now in prelude:
 * let (+>) o f = f o
 *)
let ( +!> ) refo f = refo := f !refo
(* alternatives:
 *  let ((@): 'a -> ('a -> 'b) -> 'b) = fun a b -> b a
 *  let o f g x = f (g x)
 *)

let ( $ ) f g x = g (f x)

let forever f =
  while true do
    f ()
  done

class ['a] shared_variable_hook (x : 'a) =
  object (self)
    val mutable data = x
    val mutable registered = []

    method set x =
      data <- x;
      pr "refresh registered";
      registered |> List.iter (fun f -> f ())

    method get = data
    method modify f = self#set (f self#get)
    method register f = registered <- f :: registered
  end

(* src: from aop project. was called ptFix *)
let rec fixpoint trans elem =
  let image = trans elem in
  if image =*= elem then elem (* point fixe *) else fixpoint trans image

(* le point fixe  pour les objets. was called ptFixForObjetct *)
let rec fixpoint_for_object trans elem =
  let image = trans elem in
  if image#equal elem then elem (* point fixe *)
  else fixpoint_for_object trans image

let (add_hook :
      ('a -> ('a -> 'b) -> 'b) ref -> ('a -> ('a -> 'b) -> 'b) -> unit) =
 fun var f ->
  let oldvar = !var in
  var := fun arg k -> f arg (fun x -> oldvar x k)

let (add_hook_action : ('a -> unit) -> ('a -> unit) list ref -> unit) =
 fun f hooks -> Stack_.push f hooks

let (run_hooks_action : 'a -> ('a -> unit) list ref -> unit) =
 fun obj hooks ->
  !hooks
  |> List.iter (fun f ->
         try f obj with
         | _ -> ())

type 'a mylazy = unit -> 'a

(* a la emacs.
 * bugfix: add finalize, otherwise exns can mess up the reference
 *)
let save_excursion reference newv f =
  let old = !reference in
  reference := newv;
  Common.finalize f (fun _ -> reference := old)

let save_excursion_and_disable reference f =
  save_excursion reference false (fun () -> f ())

let save_excursion_and_enable reference f =
  save_excursion reference true (fun () -> f ())

let memoized ?(use_cache = true) h k f =
  if not use_cache then f ()
  else
    try Hashtbl.find h k with
    | Not_found ->
        let v = f () in
        Hashtbl.add h k v;
        v

let cache_in_ref myref f =
  match !myref with
  | Some e -> e
  | None ->
      let e = f () in
      myref := Some e;
      e

let oncef f =
  let already = ref false in
  fun x ->
    if not !already then (
      already := true;
      f x)

let once aref f =
  if !aref then ()
  else (
    aref := true;
    f ())

(* cache_file, cf below *)

let before_leaving f x =
  f x;
  x

(* finalize, cf prelude *)

(*****************************************************************************)
(* Concurrency *)
(*****************************************************************************)

(* from http://en.wikipedia.org/wiki/File_locking
 *
 * "When using file locks, care must be taken to ensure that operations
 * are atomic. When creating the lock, the process must verify that it
 * does not exist and then create it, but without allowing another
 * process the opportunity to create it in the meantime. Various
 * schemes are used to implement this, such as taking advantage of
 * system calls designed for this purpose (but such system calls are
 * not usually available to shell scripts) or by creating the lock file
 * under a temporary name and then attempting to move it into place."
 *
 * => can't use 'if(not (file_exist xxx)) then create_file xxx' because
 * file_exist/create_file are not in atomic section (classic problem).
 *
 * from man open:
 *
 * "O_EXCL When used with O_CREAT, if the file already exists it
 * is an error and the open() will fail. In this context, a
 * symbolic link exists, regardless of where it points to.
 * O_EXCL is broken on NFS file systems; programs which
 * rely on it for performing locking tasks will contain a
 * race condition. The solution for performing atomic file
 * locking using a lockfile is to create a unique file on
 * the same file system (e.g., incorporating host- name and
 * pid), use link(2) to make a link to the lockfile. If
 * link(2) returns 0, the lock is successful. Otherwise,
 * use stat(2) on the unique file to check if its link
 * count has increased to 2, in which case the lock is also
 * successful."

 *)

exception FileAlreadyLocked

(* Racy if lock file on NFS!!! But still racy with recent Linux ? *)
let acquire_file_lock filename =
  pr2 ("Locking file: " ^ filename);
  try
    let _fd = UUnix.openfile filename [ Unix.O_CREAT; Unix.O_EXCL ] 0o777 in
    ()
  with
  | UUnix.Unix_error (e, fm, argm) ->
      pr2 (spf "exn Unix_error: %s %s %s\n" (Unix.error_message e) fm argm);
      raise FileAlreadyLocked

let release_file_lock filename =
  pr2 ("Releasing file: " ^ filename);
  USys.remove filename;
  ()

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)

exception Here
exception ReturnExn
exception WrongFormat of string

(* old: let _TODO () = failwith "TODO",  now via fix_caml with raise Todo *)

let internal_error s = failwith ("internal error: " ^ s)
let error_cant_have x = internal_error ("cant have this case" ^ Dumper.dump x)
let myassert cond = if cond then () else failwith "assert error"

(* before warning I was forced to do stuff like this:
 *
 * let (fixed_int_to_posmap: fixed_int -> posmap) = fun fixed ->
 * let v = ((fix_to_i fixed) / (power 2 16)) in
 * let _ = UPrintf.printf "coord xy = %d\n" v in
 * v
 *
 * The need for printf make me force to name stuff :(
 * How avoid ? use 'it' special keyword ?
 * In fact dont have to name it, use +> (fun v -> ...)  so when want
 * erase debug just have to erase one line.
 *)
let warning s v =
  pr2 ("Warning: " ^ s ^ "; value = " ^ Dumper.dump v);
  v

let exn_to_s_with_backtrace exn =
  Printexc.to_string exn ^ "\n" ^ Printexc.get_backtrace ()

(* want or of merd, but cant cos cant put die ... in b (strict call) *)
let ( ||| ) a b =
  try a with
  | _ -> b

(* emacs/lisp inspiration, (vouillon does that too in unison I think) *)

(* now in Prelude:
 * let unwind_protect f cleanup = ...
 * let finalize f cleanup =  ...
 *)

type error = Error of string

(* sometimes to get help from ocaml compiler to tell me places where
 * I should update, we sometimes need to change some type from pair
 * to triple, hence this kind of fake type.
 *)
type evotype = unit

let evoval = ()

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

let _check_stack = ref true

let check_stack_size limit =
  if !_check_stack then (
    pr2 "checking stack size (do ulimit -s 40000 if problem)";
    let rec aux i = if i =|= limit then 0 else 1 + aux (i + 1) in
    assert (aux 0 =|= limit);
    ())

let test_check_stack_size limit =
  (* bytecode: 100000000 *)
  (* native:   10000000 *)
  check_stack_size (int_of_string limit)

(* only relevant in bytecode, in native the stacklimit is the os stacklimit
 * (adjustable by ulimit -s)
 *)
let _init_gc_stack = ()
(* commented because cause pbs with js_of_ocaml
   Gc.set {(Gc.get ()) with Gc.stack_limit = 100 * 1024 * 1024}
*)

(* if process a big set of files then dont want get overflow in the middle
 * so for this we are ready to spend some extra time at the beginning that
 * could save far more later.
 *
 * On Centos 5.2 with ulimit -s 40000 I can only go up to 2000000 in
 * native mode (and it crash with ulimit -s 10000, which is what we want).
 *)
let check_stack_nbfiles nbfiles = if nbfiles > 200 then check_stack_size 2000000

(*###########################################################################*)
(* Basic types *)
(*###########################################################################*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)
let ( ==> ) b1 b2 = if b1 then b2 else true (* could use too => *)

(* superseded by another <=> below
   let (<=>) a b = if a =*= b then 0 else if a < b then -1 else 1
*)

let xor a b = not (a =*= b)

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

let string_of_char c = String.make 1 c
let is_single = String.contains ",;()[]{}_`"
let is_symbol = String.contains "!@#$%&*+./<=>?\\^|:-~"
let is_space = String.contains "\n\t "

let cbetween min max c =
  int_of_char c <= int_of_char max && int_of_char c >= int_of_char min

let is_upper = cbetween 'A' 'Z'
let is_lower = cbetween 'a' 'z'
let is_alpha c = is_upper c || is_lower c
let is_digit = cbetween '0' '9'
let string_of_chars cs = cs |> List_.map (String.make 1) |> String.concat ""

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

(* since 3.08, div by 0 raise Div_by_rezo, and not anymore a hardware trap :)*)
let ( /! ) x y =
  if y =|= 0 then (
    log "common.ml: div by 0";
    0)
  else x / y

(* now in prelude
 * let rec (do_n: int -> (unit -> unit) -> unit) = fun i f ->
 * if i = 0 then () else (f (); do_n (i-1) f)
 *)

let times f n = do_n n f

(* now in prelude
 * let rec (foldn: ('a -> int -> 'a) -> 'a -> int -> 'a) = fun f acc i ->
 * if i = 0 then acc else foldn f (f acc i) (i-1)
 *)

let sum_float = List.fold_left ( +. ) 0.0
(* in prelude: let sum_int   = List.fold_left (+) 0 *)

let pi = 3.14159265358979323846
let pi2 = pi /. 2.0
let pi4 = pi /. 4.0

(* 180 = pi *)
let (deg_to_rad : float -> float) = fun deg -> deg *. pi /. 180.0

let clampf = function
  | n when n < 0.0 -> 0.0
  | n when n > 1.0 -> 1.0
  | n -> n

let square x = x *. x
let rec power x n = if n =|= 0 then 1 else x * power x (n - 1)
let between i min max = i > min && i < max
let (between_strict : int -> int -> int -> bool) = fun a b c -> a < b && b < c
let borne ~min ~max x = if x > max then max else if x < min then min else x

let bitrange x p =
  let v = power 2 p in
  between x (-v) v

(* descendant *)
let (prime1 : int -> int option) =
 fun x ->
  let rec prime1_aux n =
    if n =|= 1 then None
    else if x / n * n =|= x then Some n
    else prime1_aux (n - 1)
  in
  if x =|= 1 then None
  else if x < 0 then failwith "negative"
  else prime1_aux (x - 1)

(* montant, better *)
let (prime : int -> int option) =
 fun x ->
  let rec prime_aux n =
    if n =|= x then None
    else if x / n * n =|= x then Some n
    else prime_aux (n + 1)
  in
  if x =|= 1 then None else if x < 0 then failwith "negative" else prime_aux 2

let sum xs = List.fold_left ( + ) 0 xs
let product = List.fold_left ( * ) 1

let decompose x =
  let rec decompose x =
    if x =|= 1 then []
    else
      match prime x with
      | None -> [ x ]
      | Some n -> n :: decompose (x / n)
  in
  assert (product (decompose x) =|= x);
  decompose x

let mysquare x = x * x
let sqr a = a *. a

type compare = Equal | Inf | Sup

let ( <=> ) a b = if a =*= b then Equal else if a < b then Inf else Sup
let ( <==> ) a b = if a =*= b then 0 else if a < b then -1 else 1

type uint = int

let int_of_stringchar s =
  fold_left_with_index
    (fun acc e i -> acc + (Char.code e * power 8 i))
    0
    (List.rev (list_of_string s))

let int_of_base s base =
  fold_left_with_index
    (fun acc e i ->
      let j = Char.code e - Char.code '0' in
      if j >= base then failwith "not in good base" else acc + (j * power base i))
    0
    (List.rev (list_of_string s))

let int_of_stringbits s = int_of_base s 2
let _ = assert (int_of_stringbits "1011" =|= (1 * 8) + (1 * 2) + (1 * 1))
let int_of_octal s = int_of_base s 8
let _ = assert (int_of_octal "017" =|= 15)

(* let int_of_hex s = int_of_base s 16, NONONONO cos 'A' - '0' does not give 10 !! *)

let int_of_all s =
  if String.length s >= 2 && String.get s 0 =$= '0' && is_digit (String.get s 1)
  then int_of_octal s
  else int_of_string s

let int64_of_string_opt s =
  try Some (Int64.of_string s) with
  | Failure _ -> None

let int64_of_string_c_octal_opt s =
  let open Common in
  if s =~ "^0\\([0-7]+\\)$" then
    let s = Common.matched1 s in
    int64_of_string_opt ("0o" ^ s)
  else int64_of_string_opt s

let int_of_string_c_octal_opt s =
  let open Common in
  if s =~ "^0\\([0-7]+\\)$" then
    let s = Common.matched1 s in
    int_of_string_opt ("0o" ^ s)
  else int_of_string_opt s

let float_of_string_opt s =
  match int64_of_string_c_octal_opt s with
  | Some i -> Some (Int64.to_float i)
  | None -> float_of_string_opt s

let ( += ) ref v = ref := !ref + v
let ( -= ) ref v = ref := !ref - v
let pourcent x total = x * 100 / total
let pourcent_float x total = float_of_int x *. 100.0 /. float_of_int total
let pourcent_float_of_floats x total = x *. 100.0 /. total
let pourcent_good_bad good bad = good * 100 / (good + bad)

let pourcent_good_bad_float good bad =
  float_of_int good *. 100.0 /. (float_of_int good +. float_of_int bad)

type 'a max_with_elem = int ref * 'a ref

let update_max_with_elem (aref, aelem) ~is_better (newv, newelem) =
  if is_better newv aref then (
    aref := newv;
    aelem := newelem)

(*****************************************************************************)
(* Numeric/overloading *)
(*****************************************************************************)

type 'a numdict =
  | NumDict of
      (('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a))

let add (NumDict (a, _m, _d, _n)) = a
let mul (NumDict (_a, m, _d, _n)) = m
let div (NumDict (_a, _m, d, _n)) = d
let neg (NumDict (_a, _m, _d, n)) = n
let numd_int = NumDict (( + ), ( * ), ( / ), ( ~- ))
let numd_float = NumDict (( +. ), ( *. ), ( /. ), ( ~-. ))

let testd dict n =
  let ( * ) x y = mul dict x y in
  let ( / ) x y = div dict x y in
  let ( + ) x y = add dict x y in
  (* Now you can define all sorts of things in terms of *, /, + *)
  let f num = num * num / (num + num) in
  f n

module ArithFloatInfix = struct
  let ( +.. ) = ( + )
  let ( -.. ) = ( - )
  let ( /.. ) = ( / )
  let ( *.. ) = ( * )
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( / ) = ( /. )
  let ( * ) = ( *. )
  let ( += ) ref v = ref := !ref + v
end

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

type 'a pair = 'a * 'a
type 'a triple = 'a * 'a * 'a

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let thd3 (_, _, z) = z
let sndthd (_a, b, c) = (b, c)
let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let pair f (x, y) = (f x, f y)
let triple f (x, y, z) = (f x, f y, f z)

(* for my ocamlbeautify script *)
(*
let snd = snd
let fst = fst
*)

let double a = (a, a)
let swap (x, y) = (y, x)

let tuple_of_list1 = function
  | [ a ] -> a
  | _ -> failwith "tuple_of_list1"

let tuple_of_list2 = function
  | [ a; b ] -> (a, b)
  | _ -> failwith "tuple_of_list2"

let tuple_of_list3 = function
  | [ a; b; c ] -> (a, b, c)
  | _ -> failwith "tuple_of_list3"

let tuple_of_list4 = function
  | [ a; b; c; d ] -> (a, b, c, d)
  | _ -> failwith "tuple_of_list4"

let tuple_of_list5 = function
  | [ a; b; c; d; e ] -> (a, b, c, d, e)
  | _ -> failwith "tuple_of_list5"

let tuple_of_list6 = function
  | [ a; b; c; d; e; f ] -> (a, b, c, d, e, f)
  | _ -> failwith "tuple_of_list6"

(*****************************************************************************)
(* Maybe *)
(*****************************************************************************)

let just = function
  | Some x -> x
  | _ -> failwith "just: pb"

let some = just

let optionise f =
  try Some (f ()) with
  | Not_found -> None

(* pixel *)
let some_or = function
  | None -> Fun.id
  | Some e -> fun _ -> e

let option_to_list = function
  | None -> []
  | Some x -> [ x ]

(* same
   let map_find f xs =
   xs +> List.map f +> List.find (function Some x -> true | None -> false)
    +> (function Some x -> x | None -> raise Impossible)
*)

let list_to_single_or_exn xs =
  match xs with
  | [] -> raise Not_found
  | _x :: _y :: _zs -> raise Common.Multi_found
  | [ x ] -> x

let rec (while_some :
          gen:(unit -> 'a option) -> f:('a -> 'b) -> unit -> 'b list) =
 fun ~gen ~f () ->
  match gen () with
  | None -> []
  | Some x ->
      let e = f x in
      let rest = while_some gen f () in
      e :: rest

(* perl idiom *)
let ( ||= ) aref vf =
  match !aref with
  | None -> aref := Some (vf ())
  | Some _ -> ()

let ( >>= ) m1 m2 =
  match m1 with
  | None -> None
  | Some x -> m2 x

(* http://roscidus.com/blog/blog/2013/10/13/ocaml-tips/#handling-option-types*)
let ( |? ) maybe default =
  match maybe with
  | Some v -> v
  | None -> Lazy.force default

(*****************************************************************************)
(* TriBool *)
(*****************************************************************************)

type bool3 = True3 | False3 | TrueFalsePb3 of string

(*****************************************************************************)
(* Regexp, can also use PCRE *)
(*****************************************************************************)

(* put before String section because String section use some =~ *)

(* let gsubst = global_replace *)

let ( ==~ ) s re = Str.string_match re s 0
let _memo_compiled_regexp = Hashtbl.create 101

let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
  in
  Str.string_match compile_re s 0

let match_func s re = candidate_match_func s re
let ( =~ ) s re = match_func s re

let string_match_substring re s =
  try
    let _i = Str.search_forward re s 0 in
    true
  with
  | Not_found -> false

(*
let _ =
  example(string_match_substring (Str.regexp "foo") "a foo b")
let _ =
  example(string_match_substring (Str.regexp "\\bfoo\\b") "a foo b")
let _ =
  example(string_match_substring (Str.regexp "\\bfoo\\b") "a\n\nfoo b")
let _ =
  example(string_match_substring (Str.regexp "\\bfoo_bar\\b") "a\n\nfoo_bar b")
*)
(* does not work :(
   let _ =
   example(string_match_substring (Str.regexp "\\bfoo_bar2\\b") "a\n\nfoo_bar2 b")
*)

let (regexp_match : string -> string -> string) =
 fun s re ->
  assert (s =~ re);
  Str.matched_group 1 s

(* beurk, side effect code, but hey, it is convenient *)
(* now in prelude
 * let (matched: int -> string -> string) = fun i s ->
 *    Str.matched_group i s
 *
 * let matched1 = fun s -> matched 1 s
 * let matched2 = fun s -> (matched 1 s, matched 2 s)
 * let matched3 = fun s -> (matched 1 s, matched 2 s, matched 3 s)
 * let matched4 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s)
 * let matched5 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)
 * let matched6 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)
 *)

let split sep s = Str.split (Str.regexp sep) s

(*
let _ = example (split "/" "" =*= [])
let _ = example (split ":" ":a:b" =*= ["a";"b"])
*)
let join sep xs = String.concat sep xs
(*
let _ = example (join "/" ["toto"; "titi"; "tata"] =$= "toto/titi/tata")
*)

(*
let rec join str = function
  | [] -> ""
  | [x] -> x
  | x::xs -> x ^ str ^ (join str xs)
*)

let split_list_regexp_noheading = "__noheading__"

let (split_list_regexp : string -> string list -> (string * string list) list) =
 fun re xs ->
  let rec split_lr_aux (heading, accu) = function
    | [] -> [ (heading, List.rev accu) ]
    | x :: xs ->
        if x =~ re then (heading, List.rev accu) :: split_lr_aux (x, []) xs
        else split_lr_aux (heading, x :: accu) xs
  in
  split_lr_aux ("__noheading__", []) xs |> fun xs ->
  if List_.hd_exn "unexpected empty list" xs =*= ("__noheading__", []) then
    List_.tl_exn "unexpected empty list" xs
  else xs

let regexp_alpha = Str.regexp "^[a-zA-Z_][A-Za-z_0-9]*$"

let all_match re s =
  let regexp = Str.regexp re in
  let res = ref [] in
  let _ =
    Str.global_substitute regexp
      (fun _s ->
        let substr = Str.matched_string s in
        assert (substr ==~ regexp);
        (* @Effect: also use it's side effect *)
        let paren_matched = matched1 substr in
        Stack_.push paren_matched res;
        "" (* @Dummy *))
      s
  in
  List.rev !res

(*
let _ = example (all_match "\\(@[A-Za-z]+\\)" "ca va @Et toi @Comment"
                  =*= ["@Et";"@Comment"])
*)

let global_replace_regexp re f_on_substr s =
  let regexp = Str.regexp re in
  Str.global_substitute regexp
    (fun _wholestr ->
      let substr = Str.matched_string s in
      f_on_substr substr)
    s

let regexp_word_str = "\\([a-zA-Z_][A-Za-z_0-9]*\\)"
let regexp_word = Str.regexp regexp_word_str
let regular_words s = all_match regexp_word_str s

let contain_regular_word s =
  let xs = regular_words s in
  List.length xs >= 1

(* This type allows to combine a serie of "regexps" to form a big
 * one representing its union which should then be optimized by the Str
 * module.
 *)
type regexp =
  | Contain of string
  | Start of string
  | End of string
  | Exact of string

let regexp_string_of_regexp x =
  match x with
  | Contain s -> ".*" ^ s ^ ".*"
  | Start s -> "^" ^ s
  | End s -> ".*" ^ s ^ "$"
  | Exact s -> s

let str_regexp_of_regexp x = Str.regexp (regexp_string_of_regexp x)

let compile_regexp_union xs =
  xs
  |> List_.map (fun x -> regexp_string_of_regexp x)
  |> join "\\|" |> Str.regexp

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

(* strings take space in memory. Better when can share the space used by
   similar strings *)
let _shareds = Hashtbl.create 100

let (shared_string : string -> string) =
 fun s ->
  try Hashtbl.find _shareds s with
  | Not_found ->
      Hashtbl.add _shareds s s;
      s

let chop = function
  | "" -> ""
  | s -> String.sub s 0 (String.length s - 1)

(* remove trailing / *)
let chop_dirsymbol = function
  | s when s =~ "\\(.*\\)/$" -> matched1 s
  | s -> s

let ( <!!> ) s (i, j) =
  String.sub s i (if j < 0 then String.length s - i + j + 1 else j - i)
(* let _ = example  ( "tototati"<!!>(3,-2) = "otat" ) *)

let ( <!> ) s i = String.get s i

(* pixel *)
let rec split_on_char c s =
  try
    let sp = String.index s c in
    String.sub s 0 sp
    :: split_on_char c (String.sub s (sp + 1) (String.length s - sp - 1))
  with
  | Not_found -> [ s ]

let quote s = "\"" ^ s ^ "\""

let unquote s =
  if s =~ "\"\\(.*\\)\"" then matched1 s
  else failwith ("unquote: the string has no quote: " ^ s)

(* easier to have this to be passed as hof, because ocaml dont have
 * haskell "section" operators
 *)
let is_blank_string s = s =~ "^\\([ \t]\\)*$"

(* src: lablgtk2/examples/entrycompletion.ml *)
let is_string_prefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

let plural i s =
  if i =|= 1 then Printf.sprintf "%d %s" i s else Printf.sprintf "%d %ss" i s

let showCodeHex xs = List.iter (fun i -> UPrintf.printf "%02x" i) xs
let take_string n s = String.sub s 0 (n - 1)
let take_string_safe n s = if n > String.length s then s else take_string n s

(* used by LFS *)
let size_mo_ko i =
  let ko = i / 1024 mod 1024 in
  let mo = i / 1024 / 1024 in
  if mo > 0 then Printf.sprintf "%dMo%dKo" mo ko else Printf.sprintf "%dKo" ko

let size_ko i =
  let ko = i / 1024 in
  Printf.sprintf "%dKo" ko

(* done in summer 2007 for julia
 * Reference: P216 of gusfeld book
 * For two strings S1 and S2, D(i,j) is defined to be the edit distance of S1[1..i] to S2[1..j]
 * So edit distance of S1 (of length n) and S2 (of length m) is D(n,m)
 *
 * Dynamic programming technique
 * base:
 * D(i,0) = i  for all i (cos to go from S1[1..i] to 0 characteres of S2 you have to delete all characters from S1[1..i]
 * D(0,j) = j  for all j (cos j characters must be inserted)
 * recurrence:
 * D(i,j) = min([D(i-1, j)+1, D(i, j - 1 + 1), D(i-1, j-1) + t(i,j)])
 * where t(i,j) is equal to 1 if S1(i) != S2(j) and  0 if equal
 * intuition = there is 4 possible action =  deletion, insertion, substitution, or match
 * so Lemma =
 *
 * D(i,j) must be one of the three
 *  D(i, j-1) + 1
 *  D(i-1, j)+1
 *  D(i-1, j-1) +
 *  t(i,j)
 *
 *
 *)
let matrix_distance s1 s2 =
  let n = String.length s1 in
  let m = String.length s2 in
  let mat = Array.make_matrix (n + 1) (m + 1) 0 in
  let t i j =
    if String.get s1 (i - 1) =$= String.get s2 (j - 1) then 0 else 1
  in
  let min3 a b c = min (min a b) c in

  for i = 0 to n do
    mat.(i).(0) <- i
  done;
  for j = 0 to m do
    mat.(0).(j) <- j
  done;
  for i = 1 to n do
    for j = 1 to m do
      mat.(i).(j) <-
        min3
          (mat.(i).(j - 1) + 1)
          (mat.(i - 1).(j) + 1)
          (mat.(i - 1).(j - 1) + t i j)
    done
  done;
  mat

let edit_distance s1 s2 =
  (matrix_distance s1 s2).(String.length s1).(String.length s2)

let _test_edit = edit_distance "vintner" "writers"
let _ = assert (edit_distance "winter" "winter" =|= 0)
let _ = assert (edit_distance "vintner" "writers" =|= 5)

(* src: http://pleac.sourceforge.net/pleac_ocaml/strings.html *)
(* We can emulate the Perl wrap function with the following function *)
let wrap ?(width = 80) s =
  let l = Str.split (Str.regexp " ") s in
  Format.pp_set_margin UFormat.str_formatter width;
  Format.pp_open_box UFormat.str_formatter 0;
  List.iter
    (fun x ->
      Format.pp_print_string UFormat.str_formatter x;
      Format.pp_print_break UFormat.str_formatter 1 0)
    l;
  UFormat.flush_str_formatter ()

let strip c s =
  let rec remove_prefix s =
    match s with
    | [] -> []
    | c' :: cs -> if c =*= c' then remove_prefix cs else c' :: cs
  in
  list_of_string s |> remove_prefix |> List.rev |> remove_prefix |> List.rev
  |> string_of_chars

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

type filename = string (* TODO could check that exist :) type sux *)

(* with sexp *)
type dirname = string

(* TODO could check that exist :) type sux *)
(* with sexp *)

(* file or dir *)
type path = string

module BasicType = struct
  type filename = string
end

let adjust_ext_if_needed filename ext =
  if String.get ext 0 <> '.' then
    failwith "I need an extension such as .c not just c";

  if not (filename =~ ".*\\" ^ ext) then filename ^ ext else filename

let replace_ext file oldext newext =
  let d, b, e = Filename_.dbe_of_filename file in
  assert (e = oldext);
  Filename_.filename_of_dbe (d, b, newext)

(* Given a file name, normalize it by removing "." and "..".  This works
 * exclusively by processing the string, not relying on any file system
 * state.
 *
 * This is intended to work on both absolute and relative paths, but I
 * have not tested the latter.
 *
 * Compared to a previous version of normalize_path,
 * this function normalizes "a/." to "a" and does not drop the leading "/"
 * on absolute paths. It also does not crash on "/..".
 *
 * author: Scott McPeak.
 *)
let normalize_path (file : string) : string =
  let is_rel = Filename.is_relative file in
  let xs = split "/" file in
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> (
        match x with
        | "." -> aux acc xs
        | ".." -> (
            match acc with
            | [] ->
                if is_rel then
                  (* Keep leading ".." on relative paths. *)
                  aux (x :: acc) xs
                else
                  (* Strip leading ".." on absolute paths.  The ".."
                   * directory entry on "/" points to "/". *)
                  aux acc xs
            | [ ".." ] ->
                (* If we kept "..", this must be a relative path that
                 * effectively starts with "..", so keep accumulating
                 * them.  ("../.." does not become "".) *)
                assert is_rel;
                aux (x :: acc) xs
            | _ -> aux (List_.tl_exn "unexpected empty list" acc) xs)
        | x -> aux (x :: acc) xs)
  in
  let xs' = aux [] xs in
  (if is_rel then "" else "/") ^ join "/" xs'

let relative_to_absolute s =
  if s = "." then USys.getcwd ()
  else if Filename.is_relative s then USys.getcwd () ^ "/" ^ s
  else s

let is_relative s = Filename.is_relative s
let is_absolute s = not (is_relative s)

(* pre: prj_path must not contain regexp symbol *)
let filename_without_leading_path prj_path s =
  let prj_path = chop_dirsymbol prj_path in
  if s = prj_path then "."
  else if s =~ "^" ^ prj_path ^ "/\\(.*\\)$" then matched1 s
  else
    failwith (spf "cant find filename_without_project_path: %s  %s" prj_path s)

(* realpath: see end of file *)

let grep_dash_v_str =
  "| grep -v /.hg/ |grep -v /CVS/ | grep -v /.git/ |grep -v /_darcs/"
  ^ "| grep -v /.svn/ | grep -v .git_annot | grep -v .marshall"

let arg_symlink () = if !UFile.follow_symlinks then " -L " else ""

let files_of_dir_or_files_no_vcs ext xs =
  xs
  |> List_.map (fun x ->
         if USys.is_directory x then
           (* nosemgrep: forbid-exec *)
           UCmd.cmd_to_list
             ("find " ^ arg_symlink () ^ x ^ " -noleaf -type f -name \"*." ^ ext
            ^ "\"" ^ grep_dash_v_str)
         else [ x ])
  |> List_.flatten

(*****************************************************************************)
(* i18n *)
(*****************************************************************************)
type langage = English | Francais | Deutsch

(* gettext ? *)

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(* maybe I should use ocamlcalendar, but I don't like all those functors ... *)

type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

type year = Year of int
type day = Day of int

type wday =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

type date_dmy = DMY of day * month * year
type hour = Hour of int
type minute = Min of int
type second = Sec of int
type time_hms = HMS of hour * minute * second
type full_date = date_dmy * time_hms

(* intervalle *)
type days = Days of int
type time_dmy = TimeDMY of day * month * year
type float_time = float

let check_date_dmy (DMY (_day, _month, _year)) = raise Common.Todo
let check_time_dmy (TimeDMY (_day, _month, _year)) = raise Common.Todo
let check_time_hms (HMS (_x, _y, _a)) = raise Common.Todo

(* ---------------------------------------------------------------------- *)

(* older code *)
let int_to_month i =
  assert (i <= 12 && i >= 1);
  match i with
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  (*
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
*)
  | _ -> raise Common.Impossible

let month_info =
  [
    (1, Jan, "Jan", "January", 31);
    (2, Feb, "Feb", "February", 28);
    (3, Mar, "Mar", "March", 31);
    (4, Apr, "Apr", "April", 30);
    (5, May, "May", "May", 31);
    (6, Jun, "Jun", "June", 30);
    (7, Jul, "Jul", "July", 31);
    (8, Aug, "Aug", "August", 31);
    (9, Sep, "Sep", "September", 30);
    (10, Oct, "Oct", "October", 31);
    (11, Nov, "Nov", "November", 30);
    (12, Dec, "Dec", "December", 31);
  ]

let week_day_info =
  [
    (0, Sunday, "Sun", "Dim", "Sunday");
    (1, Monday, "Mon", "Lun", "Monday");
    (2, Tuesday, "Tue", "Mar", "Tuesday");
    (3, Wednesday, "Wed", "Mer", "Wednesday");
    (4, Thursday, "Thu", "Jeu", "Thursday");
    (5, Friday, "Fri", "Ven", "Friday");
    (6, Saturday, "Sat", "Sam", "Saturday");
  ]

let i_to_month_h =
  month_info
  |> List_.map (fun (i, month, _monthstr, _mlong, _days) -> (i, month))

let s_to_month_h =
  month_info
  |> List_.map (fun (_i, month, monthstr, _mlong, _days) -> (monthstr, month))

let slong_to_month_h =
  month_info
  |> List_.map (fun (_i, month, _monthstr, mlong, _days) -> (mlong, month))

let month_to_s_h =
  month_info
  |> List_.map (fun (_i, month, monthstr, _mlong, _days) -> (month, monthstr))

let month_to_i_h =
  month_info
  |> List_.map (fun (i, month, _monthstr, _mlong, _days) -> (month, i))

let i_to_wday_h =
  week_day_info
  |> List_.map (fun (i, day, _dayen, _dayfr, _daylong) -> (i, day))

let wday_to_en_h =
  week_day_info
  |> List_.map (fun (_i, day, dayen, _dayfr, _daylong) -> (day, dayen))

let wday_to_fr_h =
  week_day_info
  |> List_.map (fun (_i, day, _dayen, dayfr, _daylong) -> (day, dayfr))

let month_of_string s = List.assoc s s_to_month_h
let month_of_string_long s = List.assoc s slong_to_month_h
let string_of_month s = List.assoc s month_to_s_h
let month_of_int i = List.assoc i i_to_month_h
let int_of_month m = List.assoc m month_to_i_h
let wday_of_int i = List.assoc i i_to_wday_h
let string_en_of_wday wday = List.assoc wday wday_to_en_h
let string_fr_of_wday wday = List.assoc wday wday_to_fr_h

(* ---------------------------------------------------------------------- *)

let wday_str_of_int ~langage i =
  let wday = wday_of_int i in
  match langage with
  | English -> string_en_of_wday wday
  | Francais -> string_fr_of_wday wday
  | Deutsch -> raise Common.Todo

let string_of_date_dmy (DMY (Day n, month, Year y)) =
  spf "%02d-%s-%d" n (string_of_month month) y

let date_dmy_of_string s =
  if s =~ "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)" then
    let day, month, year = matched3 s in
    DMY
      (Day (int_of_string day), month_of_string month, Year (int_of_string year))
  else failwith ("wrong dmy string: " ^ s)

let string_of_unix_time ?(langage = English) (tm : Unix.tm) =
  let y = tm.tm_year + 1900 in
  let mon = string_of_month (month_of_int (tm.tm_mon + 1)) in
  let d = tm.tm_mday in
  let h = tm.tm_hour in
  let min = tm.tm_min in
  let s = tm.tm_sec in

  let wday = wday_str_of_int ~langage tm.tm_wday in

  spf "%02d/%3s/%04d (%s) %02d:%02d:%02d" d mon y wday h min s

(* ex: 21/Jul/2008 (Lun) 21:25:12 *)
let unix_time_of_string s =
  if
    s
    =~ "\\([0-9][0-9]\\)/\\(...\\)/\\([0-9][0-9][0-9][0-9]\\) "
       ^ "\\(.*\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
  then
    let sday, smonth, syear, _sday, shour, smin, ssec = matched7 s in

    let y = s_to_i syear - 1900 in
    let mon = smonth |> month_of_string |> int_of_month |> fun i -> i - 1 in

    let tm = Unix.localtime (UUnix.time ()) in
    {
      tm with
      tm_year = y;
      tm_mon = mon;
      tm_mday = s_to_i sday;
      tm_hour = s_to_i shour;
      tm_min = s_to_i smin;
      tm_sec = s_to_i ssec;
    }
  else failwith ("unix_time_of_string: " ^ s)

let short_string_of_unix_time ?(langage = English) (tm : Unix.tm) =
  let y = tm.tm_year + 1900 in
  let mon = string_of_month (month_of_int (tm.tm_mon + 1)) in
  let d = tm.tm_mday in
  let _h = tm.tm_hour in
  let _min = tm.tm_min in
  let _s = tm.tm_sec in

  let wday = wday_str_of_int ~langage tm.tm_wday in

  spf "%02d/%3s/%04d (%s)" d mon y wday

let string_of_unix_time_lfs (time : Unix.tm) =
  spf "%02d--%s--%d" time.tm_mday
    (int_to_month (time.tm_mon + 1))
    (time.tm_year + 1900)

(* ---------------------------------------------------------------------- *)
let string_of_floattime ?langage i =
  let tm = Unix.localtime i in
  string_of_unix_time ?langage tm

let short_string_of_floattime ?langage i =
  let tm = Unix.localtime i in
  short_string_of_unix_time ?langage tm

let floattime_of_string s =
  let tm = unix_time_of_string s in
  let sec, _tm = Unix.mktime tm in
  sec

(* ---------------------------------------------------------------------- *)
let days_in_week_of_day day =
  let tm = Unix.localtime day in

  let wday = tm.Unix.tm_wday in
  let wday = if wday =|= 0 then 6 else wday - 1 in

  let mday = tm.Unix.tm_mday in

  let start_d = mday - wday in
  let end_d = mday + (6 - wday) in

  List_.enum start_d end_d
  |> List_.map (fun mday -> Unix.mktime { tm with Unix.tm_mday = mday } |> fst)

let first_day_in_week_of_day day =
  List_.hd_exn "unexpected empty list" (days_in_week_of_day day)

let last_day_in_week_of_day day = list_last (days_in_week_of_day day)

(* ---------------------------------------------------------------------- *)

(* (modified) copy paste from ocamlcalendar/src/date.ml *)
let days_month =
  [| 0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334 (*; 365*) |]

let rough_days_since_jesus (DMY (Day nday, month, Year year)) =
  let n = nday + days_month.(int_of_month month - 1) + (year * 365) in
  Days n

let is_more_recent d1 d2 =
  let (Days n1) = rough_days_since_jesus d1 in
  let (Days n2) = rough_days_since_jesus d2 in
  n1 > n2

let max_dmy d1 d2 = if is_more_recent d1 d2 then d1 else d2
let min_dmy d1 d2 = if is_more_recent d1 d2 then d2 else d1
let maximum_dmy ds = foldl1 max_dmy ds
let minimum_dmy ds = foldl1 min_dmy ds

let rough_days_between_dates d1 d2 =
  let (Days n1) = rough_days_since_jesus d1 in
  let (Days n2) = rough_days_since_jesus d2 in
  Days (n2 - n1)

let _ =
  assert (
    rough_days_between_dates
      (DMY (Day 7, Jan, Year 1977))
      (DMY (Day 13, Jan, Year 1977))
    =*= Days 6)

(* because of rough days, it is a bit buggy, here it should return 1 *)
(*
let _ = assert_equal
  (rough_days_between_dates
      (DMY (Day 29, Feb, Year 1977))
      (DMY (Day 1, Mar , Year 1977)))
  (Days 1)
*)

(* from julia, in gitsort.ml *)

(*
let antimonths =
  [(1,31);(2,28);(3,31);(4,30);(5,31); (6,6);(7,7);(8,31);(9,30);(10,31);
    (11,30);(12,31);(0,31)]

let normalize (year,month,day,hour,minute,second) =
  if hour < 0
  then
    let (day,hour) = (day - 1,hour + 24) in
    if day = 0
    then
      let month = month - 1 in
      let day = List.assoc month antimonths in
      let day =
  if month = 2 && year / 4 * 4 = year && not (year / 100 * 100 = year)
  then 29
  else day in
      if month = 0
      then (year-1,12,day,hour,minute,second)
      else (year,month,day,hour,minute,second)
    else (year,month,day,hour,minute,second)
  else (year,month,day,hour,minute,second)

*)

let mk_date_dmy day month year =
  let date = DMY (Day day, month_of_int month, Year year) in
  (* check_date_dmy date *)
  date

(* ---------------------------------------------------------------------- *)
(* conversion to unix.tm *)

let dmy_to_unixtime (DMY (Day n, month, Year year)) =
  let tm =
    {
      Unix.tm_sec = 0;
      (* Seconds 0..60 *)
      tm_min = 0;
      (* Minutes 0..59 *)
      tm_hour = 12;
      (* Hours 0..23 *)
      tm_mday = n;
      (* Day of month 1..31 *)
      tm_mon = int_of_month month - 1;
      (* Month of year 0..11 *)
      tm_year = year - 1900;
      (* Year - 1900 *)
      tm_wday = 0;
      (* Day of week (Sunday is 0) *)
      tm_yday = 0;
      (* Day of year 0..365 *)
      tm_isdst = false (* Daylight time savings in effect *);
    }
  in
  Unix.mktime tm

let unixtime_to_dmy tm =
  let n = tm.Unix.tm_mday in
  let month = month_of_int (tm.Unix.tm_mon + 1) in
  let year = tm.Unix.tm_year + 1900 in

  DMY (Day n, month, Year year)

let unixtime_to_floattime tm = Unix.mktime tm |> fst
let floattime_to_unixtime sec = Unix.localtime sec
let floattime_to_dmy sec = sec |> floattime_to_unixtime |> unixtime_to_dmy

let sec_to_days sec =
  let minfactor = 60 in
  let hourfactor = 60 * 60 in
  let dayfactor = 60 * 60 * 24 in

  let days = sec / dayfactor in
  let hours = sec mod dayfactor / hourfactor in
  let mins = sec mod hourfactor / minfactor in
  let sec = sec mod 60 in
  (* old:   Printf.sprintf "%d days, %d hours, %d minutes" days hours mins *)
  (if days > 0 then plural days "day" ^ " " else "")
  ^ (if hours > 0 then plural hours "hour" ^ " " else "")
  ^ (if mins > 0 then plural mins "min" ^ " " else "")
  ^ spf "%dsec" sec

let sec_to_hours sec =
  let minfactor = 60 in
  let hourfactor = 60 * 60 in

  let hours = sec / hourfactor in
  let mins = sec mod hourfactor / minfactor in
  let sec = sec mod 60 in
  (* old:   Printf.sprintf "%d days, %d hours, %d minutes" days hours mins *)
  (if hours > 0 then plural hours "hour" ^ " " else "")
  ^ (if mins > 0 then plural mins "min" ^ " " else "")
  ^ spf "%dsec" sec

let _test_date_1 () =
  let date = DMY (Day 17, Sep, Year 1991) in
  let float, _tm = dmy_to_unixtime date in
  pr2 (spf "date: %.0f" float);
  ()

(* src: ferre in logfun/.../date.ml *)

let day_secs : float = 86400.
let today : unit -> float = fun () -> UUnix.time ()
let yesterday : unit -> float = fun () -> UUnix.time () -. day_secs
let tomorrow : unit -> float = fun () -> UUnix.time () +. day_secs
let lastweek : unit -> float = fun () -> UUnix.time () -. (7.0 *. day_secs)
let lastmonth : unit -> float = fun () -> UUnix.time () -. (30.0 *. day_secs)
let week_before : float_time -> float_time = fun d -> d -. (7.0 *. day_secs)
let month_before : float_time -> float_time = fun d -> d -. (30.0 *. day_secs)
let week_after : float_time -> float_time = fun d -> d +. (7.0 *. day_secs)

let timestamp () =
  let now = UUnix.time () in
  let tm = floattime_to_unixtime now in

  let d = tm.tm_mday in
  let h = tm.tm_hour in
  let min = tm.tm_min in
  let s = tm.tm_sec in
  (* old: string_of_unix_time tm *)
  spf "%02d %02d:%02d:%02d" d h min s

(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

(* now in prelude:
 * let (list_of_string: string -> char list) = fun s ->
 * (enum 0 ((String.length s) - 1) +> List.map (String.get s))
 *)

let _ = assert (list_of_string "abcd" =*= [ 'a'; 'b'; 'c'; 'd' ])

(*
let rec (list_of_stream: ('a Stream.t) -> 'a list) =
parser
  | [< 'c ; stream >]  -> c :: list_of_stream stream
  | [<>]               -> []

let (list_of_string: string -> char list) =
  Stream.of_string $ list_of_stream
*)

(* now in prelude:
 * let (lines: string -> string list) = fun s -> ...
 *)

let (lines_with_nl : string -> string list) =
 fun s ->
  let rec lines_aux = function
    | [] -> []
    | [ x ] -> if x = "" then [] else [ x ^ "\n" ] (* old: [x] *)
    | x :: xs ->
        let e = x ^ "\n" in
        e :: lines_aux xs
  in
  time_func (fun () -> Str.split_delim (Str.regexp "\n") s) |> lines_aux

(* in fact better make it return always complete lines, simplify *)
(*  Str.split, but lines "\n1\n2\n" dont return the \n and forget the first \n => split_delim better than split *)
(* +> List.map (fun s -> s ^ "\n") but add an \n even at the end => lines_aux *)
(* old: slow
   let chars = list_of_string s in
   chars +> List.fold_left (fun (acc, lines) char ->
    let newacc = acc ^ (String.make 1 char) in
    if char = '\n'
    then ("", newacc::lines)
    else (newacc, lines)
    ) ("", [])
       +> (fun (s, lines) -> List.rev (s::lines))
*)

(*  CHECK: unlines (lines x) = x *)
let (unlines : string list -> string) = fun s -> String.concat "\n" s ^ "\n"

let (words : string -> string list) =
 fun s -> Str.split (Str.regexp "[ \t()\";]+") s

let (unwords : string list -> string) = fun s -> String.concat "" s

let (split_space : string -> string list) =
 fun s -> Str.split (Str.regexp "[ \t\n]+") s

let n_space n = repeat " " n |> join ""

let indent_string n s =
  let xs = lines s in
  xs |> List_.map (fun s -> n_space n ^ s) |> unlines

(* see nblines_eff for a more efficient implementation *)
let nblines s = lines s |> List.length
(*
let _ = example (nblines "" =|= 0)
let _ = example (nblines "toto" =|= 1)
let _ = example (nblines "toto\n" =|= 1)
let _ = example (nblines "toto\ntata" =|= 2)
let _ = example (nblines "toto\ntata\n" =|= 2)
*)

(* old: fork sucks.
 * (* note: on MacOS wc outputs some spaces before the number of lines *)
 *)

(* from https://gist.github.com/jaspervdj/1162402 *)
(* Fold over a file in chunks *)
let fold_file f x file_name =
  let buffer = Bytes.create 1024 in
  let file = UStdlib.open_in file_name in
  let rec go a =
    let length = input file buffer 0 (Bytes.length buffer) in
    let a' = f a (Bytes.sub buffer 0 length) in
    if length > 0 then go a' else a'
  in
  let r = go x in
  close_in file;
  r

(* Count the number of newlines in a buffer *)
let count_newlines s =
  let rec go n i =
    try
      let i' = Bytes.index_from s i '\n' in
      go (n + 1) (i' + 1)
    with
    | Not_found -> n
  in
  go 0 0

(* Compose the previous two functions to count the lines in a file *)
let nblines_eff file = fold_file (fun x s -> x + count_newlines s) 0 file

(* old: this could generate some Sys_error "Out of memory" in stressful
 * conditions because of the repeated calls to input_line which on
 * huge files will allocate each time new memory. The GC will reclaim
 * it, but it may be too late and we reach the physical memory limit.
 *)
(*
let nblines_eff2 file =
  let res = ref 0 in
  let finished = ref false in
  let ch = open_in_bin file in
  while not !finished do
    try
      let _ = input_line ch in
      incr res
    with End_of_file -> finished := true
  done;
  close_in ch;
  !res
*)

(* could be in h_files-format *)
let words_of_string_with_newlines s =
  lines s |> List_.map words |> List_.flatten |> exclude String_.empty

let lines_with_nl_either s =
  let xs = Str.full_split (Str.regexp "\n") s in
  xs
  |> List_.map (function
       | Str.Delim _s -> Either.Right ()
       | Str.Text s -> Either.Left s)

(*
let _ = example (lines_with_nl_either "ab\n\nc" =*=
    [Left "ab"; Right (); Right (); Left "c"])
*)

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)
let cat_orig file =
  let chan = UStdlib.open_in_bin file in
  let rec cat_orig_aux () =
    try
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
      let l = Common.input_text_line chan in
      l :: cat_orig_aux ()
    with
    | End_of_file -> []
  in
  cat_orig_aux ()

(* tail recursive efficient version *)
let cat file =
  let chan = UStdlib.open_in_bin file in
  let rec cat_aux acc () =
    (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let b, l =
      try (true, Common.input_text_line chan) with
      | End_of_file -> (false, "")
    in
    if b then cat_aux (l :: acc) () else acc
  in
  cat_aux [] () |> List.rev |> fun x ->
  close_in chan;
  x

(* Spec for cat_excerpts:
   let cat_excerpts file lines =
   let arr = cat_array file in
   lines |> List.map (fun i -> arr.(i))
*)

let cat_excerpts file lines =
  UFile.Legacy.with_open_infile file (fun chan ->
      let lines = List.sort compare lines in
      let rec aux acc lines count =
        let b, l =
          try (true, Common.input_text_line chan) with
          | End_of_file -> (false, "")
        in
        if not b then acc
        else
          match lines with
          | [] -> acc
          | c :: cdr when c =|= count -> aux (l :: acc) cdr (count + 1)
          | _ -> aux acc lines (count + 1)
      in
      aux [] lines 1 |> List.rev)

(* could do a print_string but printf dont like print_string *)
let echo s =
  UPrintf.printf "%s" s;
  flush UStdlib.stdout;
  s

let usleep s =
  for _i = 1 to s do
    ()
  done

let nblines_with_wc a = nblines_eff a

let unix_diff file1 file2 =
  let cmd = (Cmd.Name "diff", [ "-u"; file1; file2 ]) in
  (* nosemgrep: forbid-exec *)
  match UCmd.lines_of_run ~trim:true cmd with
  | Ok (xs, _status) -> xs
  | Error (`Msg s) -> failwith (spf "unix_diff problem: %s" s)

let _batch_mode = ref false

let y_or_no msg =
  pr2 (msg ^ " [y/n] ?");
  if !_batch_mode then true
  else
    let rec aux () =
      match UStdlib.read_line () with
      | "y"
      | "yes"
      | "Y" ->
          true
      | "n"
      | "no"
      | "N" ->
          false
      | _ ->
          pr2 "answer by 'y' or 'n'";
          aux ()
    in
    aux ()

let mkdir ?(mode = 0o770) file = UUnix.mkdir file mode

(* opti? use wc -l ? *)
let nblines_file file = cat file |> List.length

(* ---------------------------------------------------------------------- *)
(* _eff variant *)
(* ---------------------------------------------------------------------- *)
let _hmemo_unix_lstat_eff = Hashtbl.create 101
let _hmemo_unix_stat_eff = Hashtbl.create 101

let unix_lstat_eff file =
  if is_absolute file then
    memoized _hmemo_unix_lstat_eff file (fun () -> UUnix.lstat file)
  else
    (* this is for efficieny reason to be able to memoize the stats *)
    failwith "must pass absolute path to unix_lstat_eff"

let unix_stat_eff file =
  if is_absolute file then
    memoized _hmemo_unix_stat_eff file (fun () -> UUnix.stat file)
  else
    (* this is for efficieny reason to be able to memoize the stats *)
    failwith "must pass absolute path to unix_stat_eff"

let filesize_eff file = (unix_lstat_eff file).st_size
let filemtime_eff file = (unix_lstat_eff file).st_mtime

let lfile_exists_eff filename =
  try
    match (unix_lstat_eff filename).st_kind with
    | Unix.S_REG
    | Unix.S_LNK ->
        true
    | _ -> false
  with
  | UUnix.Unix_error (Unix.ENOENT, _, _) -> false

let is_directory_eff file = (unix_lstat_eff file).st_kind =*= Unix.S_DIR
let is_file_eff file = (unix_lstat_eff file).st_kind =*= Unix.S_REG

let is_executable_eff file =
  let stat = unix_lstat_eff file in
  let perms = stat.st_perm in
  stat.st_kind =*= Unix.S_REG && perms land 0o011 <> 0

(* ---------------------------------------------------------------------- *)

(* src: from chailloux et al book *)
let capsule_unix f args =
  try f args with
  | UUnix.Unix_error (e, fm, argm) ->
      log
        (Printf.sprintf "exn Unix_error: %s %s %s\n" (Unix.error_message e) fm
           argm)

let (readdir_to_kind_list : string -> Unix.file_kind -> string list) =
 fun path kind ->
  USys.readdir path |> Array.to_list
  |> List.filter (fun s ->
         try
           let stat = UUnix.lstat (path ^ "/" ^ s) in
           stat.st_kind =*= kind
         with
         | UUnix.Unix_error _ ->
             pr2 ("EXN pb stating file: " ^ s);
             false)

let (readdir_to_dir_list : string -> string list) =
 fun path -> readdir_to_kind_list path Unix.S_DIR

let (readdir_to_file_list : string -> string list) =
 fun path -> readdir_to_kind_list path Unix.S_REG

let (readdir_to_link_list : string -> string list) =
 fun path -> readdir_to_kind_list path Unix.S_LNK

let (readdir_to_dir_size_list : string -> (string * int) list) =
 fun path ->
  USys.readdir path |> Array.to_list
  |> List_.filter_map (fun s ->
         let stat = UUnix.lstat (path ^ "/" ^ s) in
         if stat.st_kind =*= Unix.S_DIR then Some (s, stat.st_size) else None)

let unixname () =
  let uid = UUnix.getuid () in
  let entry = UUnix.getpwuid uid in
  entry.pw_name

(* This regex matches the directory part a glob pattern
   used below. This way we are only trying to match
   files contained in the dir specified by the pattern or subdirs,
   instead of caluclating the contents of the entire
   working directory. I.e. tests/**/*.extension would
   result in tests/ *)
let dir_regex = Str.regexp "^[^\\*]*"

let glob pattern =
  Str.search_forward dir_regex pattern 0 |> ignore;
  let dir = Str.matched_string pattern in
  let regex = pattern |> Re.Glob.glob ~anchored:true |> Re.compile in
  let files = UFile.Legacy.dir_contents dir in
  files |> List.filter (fun s -> Re.execp regex s)

let sanity_check_files_and_adjust ext files =
  let files =
    files
    |> List.filter (fun file ->
           if not (file =~ ".*\\." ^ ext) then (
             pr2 ("warning: seems not a ." ^ ext ^ " file");
             false)
           else if UFile.is_dir ~follow_symlinks:true (Fpath.v file) then (
             pr2 (spf "warning: %s is a directory" file);
             false)
           else true)
  in
  files

(* taken from mlfuse, the predecessor of ocamlfuse *)
type rwx = [ `R | `W | `X ] list

let file_perm_of : u:rwx -> g:rwx -> o:rwx -> Unix.file_perm =
 fun ~u ~g ~o ->
  let to_oct l =
    List.fold_left
      (fun acc p ->
        acc
        lor (function
              | `R -> 4
              | `W -> 2
              | `X -> 1)
              p)
      0 l
  in
  let perm = (to_oct u lsl 6) lor (to_oct g lsl 3) lor to_oct o in
  perm

(* pixel *)
let has_env _var = failwith "Common.has_env, TODO"
(*
  try
    let _ = USys.getenv var in true
  with Not_found -> false
*)

let (with_open_outfile_append :
      filename -> ((string -> unit) * out_channel -> 'a) -> 'a) =
 fun file f ->
  let chan =
    UStdlib.open_out_gen [ Open_creat; Open_append; Open_binary ] 0o666 file
  in
  let pr s = output_string chan s in
  Common.unwind_protect
    (fun () ->
      let res = f (pr, chan) in
      close_out chan;
      res)
    (fun _e -> close_out chan)

let uncat xs file =
  UFile.Legacy.with_open_outfile file (fun (pr, _chan) ->
      xs
      |> List.iter (fun s ->
             pr s;
             pr "\n"))

(*###########################################################################*)
(* Collection-like types *)
(*###########################################################################*)

(*****************************************************************************)
(* Nonempty List *)
(*****************************************************************************)

(* A type for nonempty lists *)
type 'a nonempty = Nonempty of 'a * 'a list

let ( @: ) x (Nonempty (y, xs)) = Nonempty (x, y :: xs)
let nonempty_to_list (Nonempty (x, xs)) = x :: xs

(*x: common.ml *)
(*****************************************************************************)
(* List *)
(*****************************************************************************)

(* pixel *)
let uncons l =
  ( List_.hd_exn "unexpected empty list" l,
    List_.tl_exn "unexpected empty list" l )

(* pixel *)
let safe_tl l =
  try List_.tl_exn "unexpected empty list" l with
  | _ -> []

(* in prelude
   let push l v =
   l := v :: !l
*)

let rec zip xs ys =
  match (xs, ys) with
  | [], [] -> []
  | [], _ -> failwith "zip: not same length"
  | _, [] -> failwith "zip: not same length"
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let rec zip_safe xs ys =
  match (xs, ys) with
  | [], _ -> []
  | _, [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip_safe xs ys

let unzip zs =
  List_.fold_right (fun e (xs, ys) -> (fst e :: xs, snd e :: ys)) zs ([], [])

(* Same as Common2.unzip or List.split but with triples. Tail-recursive. *)
let unzip3 l =
  let rec unzip aa bb cc = function
    | (a, b, c) :: l -> unzip (a :: aa) (b :: bb) (c :: cc) l
    | [] -> (List.rev aa, List.rev bb, List.rev cc)
  in
  unzip [] [] [] l

(* Same as Common2.unzip3 but with four. Tail-recursive. *)
let unzip4 l =
  let rec unzip aa bb cc dd = function
    | (a, b, c, d) :: l -> unzip (a :: aa) (b :: bb) (c :: cc) (d :: dd) l
    | [] -> (List.rev aa, List.rev bb, List.rev cc, List.rev dd)
  in
  unzip [] [] [] [] l

let map_withkeep f xs = xs |> List_.map (fun x -> (f x, x))

(* now in prelude
 * let rec take n xs =
 * match (n,xs) with
 * | (0,_) -> []
 * | (_,[]) -> failwith "take: not enough"
 * | (n,x::xs) -> x::take (n-1) xs
 *)

let rec take_until p = function
  | [] -> []
  | x :: xs -> if p x then [] else x :: take_until p xs

let take_while p = take_until (p $ not)

(* now in prelude: let rec drop n xs = ... *)
let _ = assert (drop 3 [ 1; 2; 3; 4 ] =*= [ 4 ])

let rec drop_while p = function
  | [] -> []
  | x :: xs -> if p x then drop_while p xs else x :: xs

let drop_until p xs = drop_while (fun x -> not (p x)) xs
let _ = assert (drop_until (fun x -> x =|= 3) [ 1; 2; 3; 4; 5 ] =*= [ 3; 4; 5 ])
let _span p xs = (take_while p xs, drop_while p xs)

let rec (span : ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p -> function
  | [] -> ([], [])
  | x :: xs ->
      if p x then
        let l1, l2 = span p xs in
        (x :: l1, l2)
      else ([], x :: xs)

let _ =
  assert (
    span (fun x -> x <= 3) [ 1; 2; 3; 4; 1; 2 ] =*= ([ 1; 2; 3 ], [ 4; 1; 2 ]))

let (span_tail_call : ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p xs ->
  let rec aux acc xs =
    match xs with
    | [] -> (List.rev acc, [])
    | x :: xs -> if p x then aux (x :: acc) xs else (List.rev acc, x :: xs)
  in
  aux [] xs

let _ =
  assert (
    span_tail_call (fun x -> x <= 3) [ 1; 2; 3; 4; 1; 2 ]
    =*= ([ 1; 2; 3 ], [ 4; 1; 2 ]))

let rec groupBy eq l =
  match l with
  | [] -> []
  | x :: xs ->
      let xs1, xs2 = List.partition (fun x' -> eq x x') xs in
      (x :: xs1) :: groupBy eq xs2

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

let group_and_count xs =
  xs |> groupBy ( =*= )
  |> List_.map (fun xs ->
         match xs with
         | x :: _rest -> (x, List.length xs)
         | [] -> raise Common.Impossible)

let (exclude_but_keep_attached : ('a -> bool) -> 'a list -> ('a * 'a list) list)
    =
 fun f xs ->
  let rec aux_filter acc = function
    | [] -> [] (* drop what was accumulated because nothing to attach to *)
    | x :: xs ->
        if f x then aux_filter (x :: acc) xs
        else (x, List.rev acc) :: aux_filter [] xs
  in
  aux_filter [] xs

let _ =
  assert (
    exclude_but_keep_attached (fun x -> x =|= 3) [ 3; 3; 1; 3; 2; 3; 3; 3 ]
    =*= [ (1, [ 3; 3 ]); (2, [ 3 ]) ])

let (group_by_post : ('a -> bool) -> 'a list -> ('a list * 'a) list * 'a list) =
 fun f xs ->
  let rec aux_filter grouped_acc acc = function
    | [] -> (List.rev grouped_acc, List.rev acc)
    | x :: xs ->
        if f x then aux_filter ((List.rev acc, x) :: grouped_acc) [] xs
        else aux_filter grouped_acc (x :: acc) xs
  in
  aux_filter [] [] xs

let _ =
  assert (
    group_by_post (fun x -> x =|= 3) [ 1; 1; 3; 2; 3; 4; 5; 3; 6; 6; 6 ]
    =*= ([ ([ 1; 1 ], 3); ([ 2 ], 3); ([ 4; 5 ], 3) ], [ 6; 6; 6 ]))

let (group_by_pre : ('a -> bool) -> 'a list -> 'a list * ('a * 'a list) list) =
 fun f xs ->
  let xs' = List.rev xs in
  let ys, unclassified = group_by_post f xs' in
  ( List.rev unclassified,
    ys |> List.rev |> List_.map (fun (xs, x) -> (x, List.rev xs)) )

let _ =
  assert (
    group_by_pre (fun x -> x =|= 3) [ 1; 1; 3; 2; 3; 4; 5; 3; 6; 6; 6 ]
    =*= ([ 1; 1 ], [ (3, [ 2 ]); (3, [ 4; 5 ]); (3, [ 6; 6; 6 ]) ]))

let rec (split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list) =
 fun p -> function
  | [] -> raise Not_found
  | x :: xs ->
      if p x then ([], x, xs)
      else
        let l1, a, l2 = split_when p xs in
        (x :: l1, a, l2)

let _ =
  assert (
    split_when (fun x -> x =|= 3) [ 1; 2; 3; 4; 1; 2 ]
    =*= ([ 1; 2 ], 3, [ 4; 1; 2 ]))

(* not so easy to come up with ... used in aComment for split_paragraph *)
let rec split_gen_when_aux f acc xs =
  match xs with
  | [] -> if List_.null acc then [] else [ List.rev acc ]
  | x :: xs -> (
      match f (x :: xs) with
      | None -> split_gen_when_aux f (x :: acc) xs
      | Some rest ->
          let before = List.rev acc in
          if List_.null before then split_gen_when_aux f [] rest
          else before :: split_gen_when_aux f [] rest)

(* could avoid introduce extra aux function by using ?(acc = []) *)
let split_gen_when f xs = split_gen_when_aux f [] xs

let _ =
  assert (
    split_gen_when
      (function
        | 42 :: xs -> Some xs
        | _ -> None)
      [ 1; 2; 42; 4; 5; 6; 42; 7 ]
    =*= [ [ 1; 2 ]; [ 4; 5; 6 ]; [ 7 ] ])

(* generate exception (Failure "tl") if there is no element satisfying p *)
let rec (skip_until : ('a list -> bool) -> 'a list -> 'a list) =
 fun p xs ->
  if p xs then xs else skip_until p (List_.tl_exn "unexpected empty list" xs)

let _ =
  assert (
    skip_until
      (function
        | 1 :: 2 :: _xs -> true
        | _ -> false)
      [ 1; 3; 4; 1; 2; 4; 5 ]
    =*= [ 1; 2; 4; 5 ])

let rec skipfirst e = function
  | [] -> []
  | e' :: l when e =*= e' -> skipfirst e l
  | l -> l

(* now in prelude:
 * let rec enum x n = ...
 *)

let index_list xs =
  if List_.null xs then [] (* enum 0 (-1) generate an exception *)
  else zip xs (List_.enum 0 (List.length xs - 1))

(* if you want to use this to show the progress while processing huge list,
 * consider instead Common_extra.progress
 *)
let index_list_and_total xs =
  let total = List.length xs in
  if List_.null xs then [] (* enum 0 (-1) generate an exception *)
  else
    zip xs (List_.enum 0 (List.length xs - 1))
    |> List_.map (fun (a, b) -> (a, b, total))

let avg_list xs =
  let sum = sum_int xs in
  float_of_int sum /. float_of_int (List.length xs)

let snoc x xs = xs @ [ x ]
let cons x xs = x :: xs

let head_middle_tail xs =
  match xs with
  | x :: y :: xs ->
      let head = x in
      let reversed = List.rev (y :: xs) in
      let tail = List_.hd_exn "unexpected empty list" reversed in
      let middle = List.rev (List_.tl_exn "unexpected empty list" reversed) in
      (head, middle, tail)
  | _ -> failwith "head_middle_tail, too small list"

let _ = assert_equal (head_middle_tail [ 1; 2; 3 ]) (1, [ 2 ], 3)
let _ = assert_equal (head_middle_tail [ 1; 3 ]) (1, [], 3)

(* now in prelude
 * let (++) = (@)
 *)

(* let (++) = (@), could do that, but if load many times the common, then pb *)
(* let (++) l1 l2 = List_.fold_right (fun x acc -> x::acc) l1 l2 *)

let remove x xs =
  let newxs = List.filter (fun y -> y <> x) xs in
  assert (List.length newxs =|= List.length xs - 1);
  newxs

let rec remove_first e xs =
  match xs with
  | [] -> raise Not_found
  | x :: xs -> if x =*= e then xs else x :: remove_first e xs

(* now in prelude
   let exclude p xs =
   List.filter (fun x -> not (p x)) xs
*)
(* now in prelude
*)

let fold_k f lastk acc xs =
  let rec fold_k_aux acc = function
    | [] -> lastk acc
    | x :: xs -> f acc x (fun acc -> fold_k_aux acc xs)
  in
  fold_k_aux acc xs

let rec list_init = function
  | [] -> raise Not_found
  | [ _x ] -> []
  | x :: y :: xs -> x :: list_init (y :: xs)

(* now in prelude:
 * let rec list_last = function
 * | [] -> raise Not_found
 * | [x] -> x
 * | x::y::xs -> list_last (y::xs)
 *)

(* pixel *)
(* now in prelude
 *   let last_n n l = List.rev (take n (List.rev l))
 *   let last l = List_.hd_exn "unexpected empty list" (last_n 1 l)
 *)

(* todo: foldl, foldr (a more consistent foldr) *)

(* start pixel *)
let iter_index f l =
  let rec iter_ n = function
    | [] -> ()
    | e :: l ->
        f e n;
        iter_ (n + 1) l
  in
  iter_ 0 l

let map_index f l =
  let rec map_ n = function
    | [] -> []
    | e :: l -> f e n :: map_ (n + 1) l
  in
  map_ 0 l

(* pixel *)
let filter_index f l =
  let rec filt i = function
    | [] -> []
    | e :: l -> if f i e then e :: filt (i + 1) l else filt (i + 1) l
  in
  filt 0 l

(* pixel *)
let do_withenv doit f env l =
  let r_env = ref env in
  let l' =
    doit
      (fun e ->
        let e', env' = f !r_env e in
        r_env := env';
        e')
      l
  in
  (l', !r_env)

(* now in prelude:
 * let fold_left_with_index f acc = ...
 *)

let map_withenv f env e = do_withenv List_.map f env e

let rec collect_accu f accu = function
  | [] -> accu
  | e :: l -> collect_accu f (List.rev_append (f e) accu) l

let collect f l = List.rev (collect_accu f [] l)

(** Groups a list into a list of equivalence classes (themselves nonempty
    lists) according to the given equality predicate. `eq` must be an
    equivalence relation for correctness.
*)
let group eq l =
  List.fold_left
    (fun grouped x ->
      match
        List.fold_left
          (fun (checked, to_add) candidate_class ->
            match (to_add, candidate_class) with
            | None, _ -> (candidate_class :: checked, None)
            | Some x, Nonempty (y, _) ->
                if eq x y then ((x @: candidate_class) :: checked, None)
                else (candidate_class :: checked, Some x))
          ([], Some x) grouped
      with
      | grouped, None -> grouped
      | grouped, Some new_class -> Nonempty (new_class, []) :: grouped)
    [] l

(* cf also List.partition *)

let fpartition p l =
  let rec part yes no = function
    | [] -> (List.rev yes, List.rev no)
    | x :: l -> (
        match p x with
        | None -> part yes (x :: no) l
        | Some v -> part (v :: yes) no l)
  in
  part [] [] l

(* end pixel *)

let rec removelast = function
  | [] -> failwith "removelast"
  | [ _ ] -> []
  | e :: l -> e :: removelast l

let rec inits = function
  | [] -> [ [] ]
  | e :: l -> [] :: List_.map (fun l -> e :: l) (inits l)

let rec tails = function
  | [] -> [ [] ]
  | _ :: xs as xxs -> xxs :: tails xs

let reverse = List.rev
let rev = List.rev
let nth = List.nth
let fold_left = List.fold_left
let rev_map = List.rev_map

(* pixel *)
let rec fold_right1 f = function
  | [] -> failwith "fold_right1"
  | [ e ] -> e
  | e :: l -> f e (fold_right1 f l)

let maximum l = foldl1 max l
let minimum l = foldl1 min l

(* do a map tail recursive, and result is reversed, it is a tail recursive map => efficient *)
let map_eff_rev f l =
  let rec map_eff_aux acc = function
    | [] -> acc
    | x :: xs -> map_eff_aux (f x :: acc) xs
  in
  map_eff_aux [] l

let acc_map f l =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs -> loop (f x :: acc) xs
  in
  loop [] l

let rec (generate : int -> 'a -> 'a list) =
 fun i el -> if i =|= 0 then [] else el :: generate (i - 1) el

let rec uniq = function
  | [] -> []
  | e :: l -> if List.mem e l then uniq l else e :: uniq l

let has_no_duplicate xs = List.length xs =|= List.length (uniq xs)
let is_set_as_list = has_no_duplicate

let rec get_duplicates xs =
  match xs with
  | [] -> []
  | x :: xs ->
      if List.mem x xs then
        x :: get_duplicates xs (* todo? could x from xs to avoid double dups?*)
      else get_duplicates xs

let rec all_assoc e = function
  | [] -> []
  | (e', v) :: l when e =*= e' -> v :: all_assoc e l
  | _ :: l -> all_assoc e l

let prepare_want_all_assoc l =
  List_.map (fun n -> (n, uniq (all_assoc n l))) (uniq (List_.map fst l))

let rotate list =
  List_.tl_exn "unexpected empty list" list
  @ [ List_.hd_exn "unexpected empty list" list ]

let or_list = List.fold_left ( || ) false
let and_list = List.fold_left ( && ) true

let rec (return_when : ('a -> 'b option) -> 'a list -> 'b) =
 fun p -> function
  | [] -> raise Not_found
  | x :: xs -> (
      match p x with
      | None -> return_when p xs
      | Some b -> b)

let rec splitAt n xs =
  if n =|= 0 then ([], xs)
  else
    match xs with
    | [] -> ([], [])
    | x :: xs ->
        let a, b = splitAt (n - 1) xs in
        (x :: a, b)

let pack n xs =
  let rec pack_aux l i = function
    | [] -> failwith "not on a boundary"
    | [ x ] -> if i =|= n then [ l @ [ x ] ] else failwith "not on a boundary"
    | x :: xs ->
        if i =|= n then (l @ [ x ]) :: pack_aux [] 1 xs
        else pack_aux (l @ [ x ]) (i + 1) xs
  in
  pack_aux [] 1 xs

let rec pack_safe n xs =
  match xs with
  | [] -> []
  | _ :: _ ->
      let a, b = splitAt n xs in
      a :: pack_safe n b

let _ = assert (pack_safe 2 [ 1; 2; 3; 4; 5 ] =*= [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ])

let chunks n xs =
  let size = List.length xs in
  let chunksize = if size mod n =|= 0 then size / n else 1 + (size / n) in
  let xxs = pack_safe chunksize xs in
  if List.length xxs <> n then failwith "chunks: impossible, wrong size";
  xxs

let _ = assert (chunks 2 [ 1; 2; 3; 4 ] =*= [ [ 1; 2 ]; [ 3; 4 ] ])
let _ = assert (chunks 2 [ 1; 2; 3; 4; 5 ] =*= [ [ 1; 2; 3 ]; [ 4; 5 ] ])

let min_with f = function
  | [] -> raise Not_found
  | e :: l ->
      let rec min_with_ min_val min_elt = function
        | [] -> min_elt
        | e :: l ->
            let val_ = f e in
            if val_ < min_val then min_with_ val_ e l
            else min_with_ min_val min_elt l
      in
      min_with_ (f e) e l

let two_mins_with f = function
  | e1 :: e2 :: l ->
      let rec min_with_ min_val min_elt min_val2 min_elt2 = function
        | [] -> (min_elt, min_elt2)
        | e :: l ->
            let val_ = f e in
            if val_ < min_val2 then
              if val_ < min_val then min_with_ val_ e min_val min_elt l
              else min_with_ min_val min_elt val_ e l
            else min_with_ min_val min_elt min_val2 min_elt2 l
      in
      let v1 = f e1 in
      let v2 = f e2 in
      if v1 < v2 then min_with_ v1 e1 v2 e2 l else min_with_ v2 e2 v1 e1 l
  | _ -> raise Not_found

let grep_with_previous f = function
  | [] -> []
  | e :: l ->
      let rec grep_with_previous_ previous = function
        | [] -> []
        | e :: l ->
            if f previous e then e :: grep_with_previous_ e l
            else grep_with_previous_ previous l
      in
      e :: grep_with_previous_ e l

let iter_with_previous f = function
  | [] -> ()
  | e :: l ->
      let rec iter_with_previous_ previous = function
        | [] -> ()
        | e :: l ->
            f previous e;
            iter_with_previous_ e l
      in
      iter_with_previous_ e l

let iter_with_previous_opt f = function
  | [] -> ()
  | e :: l ->
      f None e;
      let rec iter_with_previous_ previous = function
        | [] -> ()
        | e :: l ->
            f (Some previous) e;
            iter_with_previous_ e l
      in
      iter_with_previous_ e l

let iter_with_before_after f xs =
  let rec aux before_rev after =
    match after with
    | [] -> ()
    | x :: xs ->
        f before_rev x xs;
        aux (x :: before_rev) xs
  in
  aux [] xs

(* kind of cartesian product of x*x  *)
let rec (get_pair : 'a list -> ('a * 'a) list) = function
  | [] -> []
  | x :: xs -> List_.map (fun y -> (x, y)) xs @ get_pair xs

(* retourne le rang dans une liste d'un element *)
let rang elem liste =
  let rec rang_rec elem accu = function
    | [] -> raise Not_found
    | a :: l -> if a =*= elem then accu else rang_rec elem (accu + 1) l
  in
  rang_rec elem 1 liste

(* retourne vrai si une liste contient des doubles *)
let rec doublon = function
  | [] -> false
  | a :: l -> if List.mem a l then true else doublon l

let rec (insert_in : 'a -> 'a list -> 'a list list) =
 fun x -> function
  | [] -> [ [ x ] ]
  | y :: ys -> (x :: y :: ys) :: List_.map (fun xs -> y :: xs) (insert_in x ys)
(* insert_in 3 [1;2] = [[3; 1; 2]; [1; 3; 2]; [1; 2; 3]] *)

let rec (permutation : 'a list -> 'a list list) = function
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | x :: xs -> List.concat_map (insert_in x) (permutation xs)
(* permutation [1;2;3] =
 * [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]]
 *)

let rec remove_elem_pos pos xs =
  match (pos, xs) with
  | _, [] -> failwith "remove_elem_pos"
  | 0, _x :: xs -> xs
  | n, x :: xs -> x :: remove_elem_pos (n - 1) xs

let rec insert_elem_pos (e, pos) xs =
  match (pos, xs) with
  | 0, xs -> e :: xs
  | n, x :: xs -> x :: insert_elem_pos (e, n - 1) xs
  | _n, [] -> failwith "insert_elem_pos"

let uncons_permut xs =
  let indexed = index_list xs in
  indexed |> List_.map (fun (x, pos) -> ((x, pos), remove_elem_pos pos xs))

let _ =
  assert (
    uncons_permut [ 'a'; 'b'; 'c' ]
    =*= [
          (('a', 0), [ 'b'; 'c' ]);
          (('b', 1), [ 'a'; 'c' ]);
          (('c', 2), [ 'a'; 'b' ]);
        ])

let uncons_permut_lazy xs =
  let indexed = index_list xs in
  indexed
  |> List_.map (fun (x, pos) -> ((x, pos), lazy (remove_elem_pos pos xs)))

(* pixel *)
let map_flatten f l =
  let rec map_flatten_aux accu = function
    | [] -> accu
    | e :: l -> map_flatten_aux (List.rev (f e) @ accu) l
  in
  List.rev (map_flatten_aux [] l)

(* now in prelude: let rec repeat e n *)

let rec map2 f = function
  | [] -> []
  | x :: xs ->
      let r = f x in
      r :: map2 f xs

let map3 f l =
  let rec map3_aux acc = function
    | [] -> acc
    | x :: xs -> map3_aux (f x :: acc) xs
  in
  map3_aux [] l

(*
let tails2 xs = map rev (inits (rev xs))
let res = tails2 [1;2;3;4]
let res = tails [1;2;3;4]
let id x = x
*)

let pack_sorted same xs =
  let rec pack_s_aux acc xs =
    match (acc, xs) with
    | (cur, rest), [] -> cur :: rest
    | (cur, rest), y :: ys ->
        if same (List_.hd_exn "unexpected empty list" cur) y then
          pack_s_aux (y :: cur, rest) ys
        else pack_s_aux ([ y ], cur :: rest) ys
  in
  pack_s_aux
    ([ List_.hd_exn "unexpected empty list" xs ], [])
    (List_.tl_exn "unexpected empty list" xs)
  |> List.rev

let rec keep_best f =
  let rec partition e = function
    | [] -> (e, [])
    | e' :: l -> (
        match f (e, e') with
        | None ->
            let e'', l' = partition e l in
            (e'', e' :: l')
        | Some e'' -> partition e'' l)
  in
  function
  | [] -> []
  | e :: l ->
      let e', l' = partition e l in
      e' :: keep_best f l'

let rec sorted_keep_best f = function
  | [] -> []
  | [ a ] -> [ a ]
  | a :: b :: l -> (
      match f a b with
      | None -> a :: sorted_keep_best f (b :: l)
      | Some e -> sorted_keep_best f (e :: l))

let (cartesian_product : 'a list -> 'b list -> ('a * 'b) list) =
 fun xs ys ->
  xs |> List_.map (fun x -> ys |> List_.map (fun y -> (x, y))) |> List_.flatten

let _ =
  assert_equal
    (cartesian_product [ 1; 2 ] [ "3"; "4"; "5" ])
    [ (1, "3"); (1, "4"); (1, "5"); (2, "3"); (2, "4"); (2, "5") ]

(* TODO: add [@@profiling] *)
let sort_prof a b = List.sort a b

type order = HighFirst | LowFirst

let compare_order order a b =
  match order with
  | HighFirst -> compare b a
  | LowFirst -> compare a b

let sort_by_val_highfirst xs =
  sort_prof (fun (_k1, v1) (_k2, v2) -> compare v2 v1) xs

let sort_by_val_lowfirst xs =
  sort_prof (fun (_k1, v1) (_k2, v2) -> compare v1 v2) xs

let sort_by_key_highfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k2 k1) xs

let sort_by_key_lowfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k1 k2) xs

let _ =
  assert (sort_by_key_lowfirst [ (4, ()); (7, ()) ] =*= [ (4, ()); (7, ()) ])

let _ =
  assert (sort_by_key_highfirst [ (4, ()); (7, ()) ] =*= [ (7, ()); (4, ()) ])

let sortgen_by_key_highfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k2 k1) xs

let sortgen_by_key_lowfirst xs =
  sort_prof (fun (k1, _v1) (k2, _v2) -> compare k1 k2) xs

(*----------------------------------*)

(* sur surEnsemble [p1;p2] [[p1;p2;p3] [p1;p2] ....] -> [[p1;p2;p3] ...      *)
(* mais pas p2;p3                                                            *)
(* (aop) *)
let surEnsemble liste_el liste_liste_el =
  List.filter
    (function
      | liste_elbis ->
          List.for_all
            (function
              | el -> List.mem el liste_elbis)
            liste_el)
    liste_liste_el

(*----------------------------------*)
(* combinaison/product/.... (aop) *)
(* 123 -> 123 12 13 23 1 2 3 *)
let rec realCombinaison = function
  | [] -> []
  | [ a ] -> [ [ a ] ]
  | a :: l ->
      let res = realCombinaison l in
      let res2 =
        List_.map
          (function
            | x -> a :: x)
          res
      in
      res2 @ res @ [ [ a ] ]

(* genere toutes les combinaisons possible de paire      *)
(* par exemple combinaison [1;2;4] -> [1, 2; 1, 4; 2, 4] *)
let rec combinaison = function
  | [] -> []
  | [ _a ] -> []
  | [ a; b ] -> [ (a, b) ]
  | a :: b :: l ->
      List_.map
        (function
          | elem -> (a, elem))
        (b :: l)
      @ combinaison (b :: l)

(*----------------------------------*)

(* list of list(aop) *)
(* insere elem dans la liste de liste (si elem est deja present dans une de  *)
(* ces listes, on ne fait rien                                               *)
let rec insere elem = function
  | [] -> [ [ elem ] ]
  | a :: l -> if List.mem elem a then a :: l else a :: insere elem l

let rec insereListeContenant lis el = function
  | [] -> [ el :: lis ]
  | a :: l ->
      if List.mem el a then List.append lis a :: l
      else a :: insereListeContenant lis el l

(* fusionne les listes contenant et1 et et2  dans la liste de liste*)
let rec fusionneListeContenant (et1, et2) = function
  | [] -> [ [ et1; et2 ] ]
  | a :: l ->
      (* si les deux sont deja dedans alors rien faire *)
      if List.mem et1 a then
        if List.mem et2 a then a :: l else insereListeContenant a et2 l
      else if List.mem et2 a then insereListeContenant a et1 l
      else a :: fusionneListeContenant (et1, et2) l

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

(* do bound checking ? *)
let array_find_index f _a =
  let rec array_find_index_ i = if f i then i else array_find_index_ (i + 1) in
  try array_find_index_ 0 with
  | _ -> raise Not_found

let array_find_index_via_elem f a =
  let rec array_find_index_ i =
    if f a.(i) then i else array_find_index_ (i + 1)
  in
  try array_find_index_ 0 with
  | _ -> raise Not_found

type idx = Idx of int

let next_idx (Idx i) = Idx (i + 1)
let int_of_idx (Idx i) = i

let array_find_index_typed f _a =
  let rec array_find_index_ i =
    if f i then i else array_find_index_ (next_idx i)
  in
  try array_find_index_ (Idx 0) with
  | _ -> raise Not_found

(*****************************************************************************)
(* Matrix *)
(*****************************************************************************)

type 'a matrix = 'a array array

let map_matrix f mat = mat |> Array.map (fun arr -> arr |> Array.map f)

let (make_matrix_init :
      nrow:int -> ncolumn:int -> (int -> int -> 'a) -> 'a matrix) =
 fun ~nrow ~ncolumn f ->
  Array.init nrow (fun i -> Array.init ncolumn (fun j -> f i j))

let iter_matrix f m =
  Array.iteri (fun i e -> Array.iteri (fun j x -> f i j x) e) m

let nb_rows_matrix m = Array.length m

let nb_columns_matrix m =
  assert (Array.length m > 0);
  Array.length m.(0)

let (rows_of_matrix : 'a matrix -> 'a list list) =
 fun m -> Array.to_list m |> List_.map Array.to_list

let (columns_of_matrix : 'a matrix -> 'a list list) =
 fun m ->
  let nbcols = nb_columns_matrix m in
  let nbrows = nb_rows_matrix m in
  List_.enum 0 (nbcols - 1)
  |> List_.map (fun j ->
         List_.enum 0 (nbrows - 1) |> List_.map (fun i -> m.(i).(j)))

let all_elems_matrix_by_row m = rows_of_matrix m |> List_.flatten
let ex_matrix1 = [| [| 0; 1; 2 |]; [| 3; 4; 5 |]; [| 6; 7; 8 |] |]
let ex_rows1 = [ [ 0; 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7; 8 ] ]
let ex_columns1 = [ [ 0; 3; 6 ]; [ 1; 4; 7 ]; [ 2; 5; 8 ] ]
let _ = assert (rows_of_matrix ex_matrix1 =*= ex_rows1)
let _ = assert (columns_of_matrix ex_matrix1 =*= ex_columns1)

(*****************************************************************************)
(* Fast array *)
(*****************************************************************************)
(*
module B_Array = Bigarray.Array2
*)

(*
open B_Array
open Bigarray
*)

(* for the string_of auto generation of camlp4
   val b_array_string_of_t : 'a -> 'b -> string
   val bigarray_string_of_int16_unsigned_elt : 'a -> string
   val bigarray_string_of_c_layout : 'a -> string
   let b_array_string_of_t f a = "<>"
   let bigarray_string_of_int16_unsigned_elt a = "<>"
   let bigarray_string_of_c_layout a = "<>"
*)

(*****************************************************************************)
(* Set. Have a look too at set*.mli  *)
(*****************************************************************************)
type 'a set = 'a list
(* with sexp *)

let (empty_set : 'a set) = []

let (insert_set : 'a -> 'a set -> 'a set) =
 fun x xs ->
  if List.mem x xs then
    (* let _ = print_string "warning insert: already exist" in *)
    xs
  else x :: xs

let is_set xs = has_no_duplicate xs
let (single_set : 'a -> 'a set) = fun x -> insert_set x empty_set

let (set : 'a list -> 'a set) =
 fun xs -> xs |> List.fold_left (flip insert_set) empty_set |> List.sort compare

let (exists_set : ('a -> bool) -> 'a set -> bool) = List.exists
let (forall_set : ('a -> bool) -> 'a set -> bool) = List.for_all
let (filter_set : ('a -> bool) -> 'a set -> 'a set) = List.filter
let (fold_set : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a) = List.fold_left
let (map_set : ('a -> 'b) -> 'a set -> 'b set) = List_.map
let (member_set : 'a -> 'a set -> bool) = List.mem
let find_set = List.find
let sort_set = List.sort
let iter_set = List.iter
let top_set (xs : 'a set) : 'a = List_.hd_exn "unexpected empty list" xs

let (inter_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 ->
  s1
  |> fold_set
       (fun acc x -> if member_set x s2 then insert_set x acc else acc)
       empty_set

let (union_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 ->
  s2
  |> fold_set
       (fun acc x -> if member_set x s1 then acc else insert_set x acc)
       s1

let (minus_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 -> s1 |> filter_set (fun x -> not (member_set x s2))

let union_all l = List.fold_left union_set [] l
let big_union_set f xs = xs |> map_set f |> fold_set union_set empty_set
let (card_set : 'a set -> int) = List.length

let (include_set : 'a set -> 'a set -> bool) =
 fun s1 s2 -> s1 |> forall_set (fun p -> member_set p s2)

let equal_set s1 s2 = include_set s1 s2 && include_set s2 s1

let (include_set_strict : 'a set -> 'a set -> bool) =
 fun s1 s2 -> card_set s1 < card_set s2 && include_set s1 s2

let ( $*$ ) = inter_set
let ( $+$ ) = union_set
let ( $-$ ) = minus_set
let ( $?$ ) a b = member_set a b
let ( $<$ ) = include_set_strict
let ( $<=$ ) = include_set
let ( $=$ ) = equal_set

(* as $+$ but do not check for memberness, allow to have set of func *)
let ( $@$ ) a b = a @ b

let rec nub = function
  | [] -> []
  | x :: xs -> if List.mem x xs then nub xs else x :: nub xs

(*****************************************************************************)
(* Set as normal list *)
(*****************************************************************************)
(*
let (union: 'a list -> 'a list -> 'a list) = fun l1 l2 ->
  List.fold_left (fun acc x -> if List.mem x l1 then acc else x::acc) l1 l2

let insert_normal x xs = union xs [x]

(* retourne lis1 - lis2 *)
let minus l1 l2 = List.filter    (fun x -> not (List.mem x l2)) l1

let inter l1 l2 = List.fold_left (fun acc x -> if List.mem x l2 then x::acc else acc) [] l1

let union_list =  List.fold_left union []

let uniq lis =
  List.fold_left (function acc -> function el -> union [el] acc) [] lis

(* pixel *)
let rec non_uniq = function
  | [] -> []
  | e::l -> if mem e l then e :: non_uniq l else non_uniq l

let rec inclu lis1 lis2 =
  List.for_all (function el -> List.mem el lis2) lis1

let equivalent lis1 lis2 =
  (inclu lis1 lis2) && (inclu lis2 lis1)

*)

(*****************************************************************************)
(* Set as sorted list *)
(*****************************************************************************)
(* liste trie, cos we need to do intersection, and insertion (it is a set
   cos when introduce has, if we create a new has => must do a recurse_rep
   and another categ can have to this has => must do an union
*)
(*
let rec insert x = function
  | [] -> [x]
  | y::ys ->
      if x = y then y::ys
      else (if x < y then x::y::ys else y::(insert x ys))

(* same, suppose sorted list *)
let rec intersect x y =
  match(x,y) with
  | [], y -> []
  | x,  [] -> []
  | x::xs, y::ys ->
      if x = y then x::(intersect xs ys)
      else
  (if x < y then intersect xs (y::ys)
  else intersect (x::xs) ys
 )
(* intersect [1;3;7] [2;3;4;7;8];;   *)
*)

(*****************************************************************************)
(* Sets specialized *)
(*****************************************************************************)

(* people often do that *)
module StringSetOrig = Set.Make (struct
  type t = string

  let compare = String.compare
end)

module StringSet = struct
  include StringSetOrig

  let of_list xs =
    xs
    |> List.fold_left (fun acc e -> StringSetOrig.add e acc) StringSetOrig.empty

  let to_list t = StringSetOrig.elements t
end

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
type ('a, 'b) assoc = ('a * 'b) list
(* with sexp *)

let (assoc_to_function : ('a, 'b) assoc -> 'a -> 'b) =
 fun xs ->
  xs
  |> List.fold_left
       (fun acc (k, v) k' -> if k =*= k' then v else acc k')
       (fun _k -> failwith "no key in this assoc")
(* simpler:
   let (assoc_to_function: ('a, 'b) assoc -> ('a -> 'b)) = fun xs ->
   fun k -> List.assoc k xs
*)

let (empty_assoc : ('a, 'b) assoc) = []
let fold_assoc = List.fold_left
let insert_assoc x xs = x :: xs
let map_assoc = List_.map
let filter_assoc = List.filter
let assoc = List.assoc
let keys xs = List_.map fst xs
let lookup = assoc

(* assert unique key ?*)
let del_assoc key xs = xs |> List.filter (fun (k, _v) -> k <> key)
let replace_assoc (key, v) xs = insert_assoc (key, v) (del_assoc key xs)

let apply_assoc key f xs =
  let old = assoc key xs in
  replace_assoc (key, f old) xs

let big_union_assoc f xs = xs |> map_assoc f |> fold_assoc union_set empty_set

(* todo: pb normally can suppr fun l -> .... l but if do that, then strange type _a
   => assoc_map is strange too => equal dont work
*)
let (assoc_reverse : ('a * 'b) list -> ('b * 'a) list) =
 fun l -> List_.map (fun (x, y) -> (y, x)) l

let (assoc_map : ('a * 'b) list -> ('a * 'b) list -> ('a * 'a) list) =
 fun l1 l2 ->
  let l1bis, l2bis = (assoc_reverse l1, assoc_reverse l2) in
  List_.map (fun (x, y) -> (y, List.assoc x l2bis)) l1bis

let rec (lookup_list : 'a -> ('a, 'b) assoc list -> 'b) =
 fun el -> function
  | [] -> raise Not_found
  | xs :: xxs -> (
      try List.assoc el xs with
      | Not_found -> lookup_list el xxs)

let (lookup_list2 : 'a -> ('a, 'b) assoc list -> 'b * int) =
 fun el xxs ->
  let rec lookup_l_aux i = function
    | [] -> raise Not_found
    | xs :: xxs -> (
        try
          let res = List.assoc el xs in
          (res, i)
        with
        | Not_found -> lookup_l_aux (i + 1) xxs)
  in
  lookup_l_aux 0 xxs

let _ =
  assert (
    lookup_list2 "c"
      [ [ ("a", 1); ("b", 2) ]; [ ("a", 1); ("b", 3) ]; [ ("a", 1); ("c", 7) ] ]
    =*= (7, 2))

let assoc_opt k l = optionise (fun () -> List.assoc k l)

let assoc_with_err_msg k l =
  try List.assoc k l with
  | Not_found ->
      pr2 (spf "pb assoc_with_err_msg: %s" (Dumper.dump k));
      raise Not_found

(*****************************************************************************)
(* Assoc int -> xxx with binary tree.  Have a look too at Mapb.mli *)
(*****************************************************************************)

(* ex: type robot_list = robot_info IntMap.t *)
module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

let intmap_to_list m = IntMap.fold (fun id v acc -> (id, v) :: acc) m []
let intmap_string_of_t _f _a = "<Not Yet>"

module IntIntMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let intintmap_to_list m = IntIntMap.fold (fun id v acc -> (id, v) :: acc) m []
let intintmap_string_of_t _f _a = "<Not Yet>"

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

(* il parait que better  when choose a prime *)
let hcreate () = Hashtbl.create 401
let hadd (k, v) h = Hashtbl.add h k v
let hmem k h = Hashtbl.mem h k
let hfind k h = Hashtbl.find h k
let hreplace (k, v) h = Hashtbl.replace h k v
let hiter = Hashtbl.iter
let hfold = Hashtbl.fold
let hremove k h = Hashtbl.remove h k

let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] |> List.sort compare

let hash_to_list_unsorted h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  (* replace or add? depends the semantic of hashtbl you want *)
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

(*
let _  =
  let h = Hashtbl.create 101 in
  Hashtbl.add h "toto" 1;
  Hashtbl.add h "toto" 1;
  assert(hash_to_list h =*= ["toto",1; "toto",1])
*)

let hfind_default key value_if_not_found h =
  try Hashtbl.find h key with
  | Not_found ->
      Hashtbl.add h key (value_if_not_found ());
      Hashtbl.find h key

(* not as easy as Perl  $h->{key}++; but still possible *)
let hupdate_default key ~update:op ~default:value_if_not_found h =
  let old = hfind_default key value_if_not_found h in
  Hashtbl.replace h key (op old)

let add1 old = old + 1
let cst_zero () = 0
let hfind_option key h = optionise (fun () -> Hashtbl.find h key)

(* see below: let hkeys h = ... *)

let count_elements_sorted_highfirst xs =
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun e -> hupdate_default e (fun old -> old + 1) (fun () -> 0) h);
  let xs = hash_to_list h in
  (* not very efficient ... but simpler. use max_with_elem stuff ? *)
  sort_by_val_highfirst xs

let most_recurring_element xs =
  let xs' = count_elements_sorted_highfirst xs in
  match xs' with
  | (e, _count) :: _ -> e
  | [] -> failwith "most_recurring_element: empty list"

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t
(* with sexp *)

let hash_hashset_add k e h =
  match optionise (fun () -> Hashtbl.find h k) with
  | Some hset -> Hashtbl.replace hset e true
  | None ->
      let hset = Hashtbl.create 11 in
      Hashtbl.add h k hset;
      Hashtbl.replace hset e true

let hashset_to_set baseset h =
  h |> hash_to_list |> List_.map fst |> fun xs -> baseset#fromlist xs

let hashset_to_list h = hash_to_list h |> List_.map fst
let hashset_of_list xs = xs |> List_.map (fun x -> (x, true)) |> hash_of_list

let hashset_union h1 h2 =
  h2 |> Hashtbl.iter (fun k _bool -> Hashtbl.replace h1 k true)

let hashset_inter h1 h2 =
  h1
  |> Hashtbl.iter (fun k _bool ->
         if not (Hashtbl.mem h2 k) then Hashtbl.remove h1 k)

let hkeys h =
  let hkey = Hashtbl.create 101 in
  h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
  hashset_to_list hkey

let hunion h1 h2 = h2 |> Hashtbl.iter (fun k v -> Hashtbl.add h1 k v)

let group_assoc_bykey_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl_.push h k v);
  let keys = hkeys h in
  keys |> List_.map (fun k -> (k, Hashtbl_.get_stack h k))

let _test_group_assoc () =
  let xs = List_.enum 0 10000 |> List_.map (fun i -> (i_to_s i, i)) in
  let xs = ("0", 2) :: xs in
  (*    let _ys = xs +> Common.groupBy (fun (a,resa) (b,resb) -> a = b)  *)
  let ys = xs |> group_assoc_bykey_eff in
  pr2_gen ys

let uniq_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun k -> Hashtbl.replace h k true);
  hkeys h

let big_union_eff xxs =
  let h = Hashtbl.create 101 in

  xxs
  |> List.iter (fun xs -> xs |> List.iter (fun k -> Hashtbl.replace h k true));
  hkeys h

let diff_set_eff xs1 xs2 =
  let h1 = hashset_of_list xs1 in
  let h2 = hashset_of_list xs2 in

  let hcommon = Hashtbl.create 101 in
  let honly_in_h1 = Hashtbl.create 101 in
  let honly_in_h2 = Hashtbl.create 101 in

  h1
  |> Hashtbl.iter (fun k _ ->
         if Hashtbl.mem h2 k then Hashtbl.replace hcommon k true
         else Hashtbl.add honly_in_h1 k true);
  h2
  |> Hashtbl.iter (fun k _ ->
         if Hashtbl.mem h1 k then Hashtbl.replace hcommon k true
         else Hashtbl.add honly_in_h2 k true);
  ( hashset_to_list hcommon,
    hashset_to_list honly_in_h1,
    hashset_to_list honly_in_h2 )

(*****************************************************************************)
(* Hash with default value *)
(*****************************************************************************)

(*
type ('k,'v) hash_with_default = {
  h: ('k, 'v) Hashtbl.t;
  default_value: unit -> 'v;
}
*)
type ('a, 'b) hash_with_default =
  < add : 'a -> 'b -> unit
  ; to_list : ('a * 'b) list
  ; to_h : ('a, 'b) Hashtbl.t
  ; update : 'a -> ('b -> 'b) -> unit
  ; assoc : 'a -> 'b >

let hash_with_default fv =
  object
    val h = Hashtbl.create 101
    method to_list = hash_to_list h
    method to_h = h
    method add k v = Hashtbl.replace h k v
    method assoc k = Hashtbl.find h k
    method update k f = hupdate_default k ~update:f ~default:fv h
  end

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)
type 'a stack = 'a list
(* with sexp *)

let (empty_stack : 'a stack) = []

(*let (push: 'a -> 'a stack -> 'a stack) = fun x xs -> x::xs *)
let top (xs : 'a stack) : 'a = List_.hd_exn "unexpected empty stack" xs
let pop (xs : 'a stack) : 'a stack = List_.tl_exn "unexpected empty stack" xs

let top_option = function
  | [] -> None
  | x :: _xs -> Some x

(*****************************************************************************)
(* Undoable Stack *)
(*****************************************************************************)

(* Okasaki use such structure also for having efficient data structure
 * supporting fast append.
 *)

type 'a undo_stack = 'a list * 'a list (* redo *)

let (empty_undo_stack : 'a undo_stack) = ([], [])

(* push erase the possible redo *)
let (push_undo : 'a -> 'a undo_stack -> 'a undo_stack) =
 fun x (undo, _redo) -> (x :: undo, [])

let (top_undo : 'a undo_stack -> 'a) =
 fun (undo, _redo) -> List_.hd_exn "unexpected empty list" undo

let (pop_undo : 'a undo_stack -> 'a undo_stack) =
 fun (undo, redo) ->
  match undo with
  | [] -> failwith "empty undo stack"
  | x :: xs -> (xs, x :: redo)

let (undo_pop : 'a undo_stack -> 'a undo_stack) =
 fun (undo, redo) ->
  match redo with
  | [] -> failwith "empty redo, nothing to redo"
  | x :: xs -> (x :: undo, xs)

let redo_undo x = undo_pop x

let top_undo_option (undo, _redo) =
  match undo with
  | [] -> None
  | x :: _xs -> Some x

(*****************************************************************************)
(* Binary tree *)
(*****************************************************************************)

(* type 'a bintree = Leaf of 'a | Branch of ('a bintree * 'a bintree) *)

(*****************************************************************************)
(* N-ary tree *)
(*****************************************************************************)

(* no empty tree, must have one root at list *)
type 'a tree2 = Tree of 'a * 'a tree2 list

let rec (tree2_iter : ('a -> unit) -> 'a tree2 -> unit) =
 fun f tree ->
  match tree with
  | Tree (node, xs) ->
      f node;
      xs |> List.iter (tree2_iter f)

type ('a, 'b) tree = Node of 'a * ('a, 'b) tree list | Leaf of 'b
(* with tarzan *)

let rec map_tree ~fnode ~fleaf tree =
  match tree with
  | Leaf x -> Leaf (fleaf x)
  | Node (x, xs) -> Node (fnode x, xs |> List_.map (map_tree ~fnode ~fleaf))

(*****************************************************************************)
(* N-ary tree with updatable childrens *)
(*****************************************************************************)

(* no empty tree, must have one root at list *)

type 'a treeref = NodeRef of 'a * 'a treeref list ref

let treeref_children_ref tree =
  match tree with
  | NodeRef (_n, x) -> x

let rec (treeref_node_iter :
          (* (('a * ('a, 'b) treeref list ref) -> unit) ->
             ('a, 'b) treeref -> unit
          *)
          'a) =
 fun f tree ->
  match tree with
  (*  | LeafRef _ -> ()*)
  | NodeRef (n, xs) ->
      f (n, xs);
      !xs |> List.iter (treeref_node_iter f)

let find_treeref f tree =
  let res = ref [] in

  tree
  |> treeref_node_iter (fun (n, xs) ->
         if f (n, xs) then Stack_.push (n, xs) res);
  match !res with
  | [ (n, xs) ] -> NodeRef (n, xs)
  | [] -> raise Not_found
  | _x :: _y :: _zs -> raise Common.Multi_found

let (treeref_node_iter_with_parents :
      (* (('a * ('a, 'b) treeref list ref) -> ('a list) -> unit) ->
         ('a, 'b) treeref -> unit)
      *)
      'a) =
 fun f tree ->
  let rec aux acc tree =
    match tree with
    (*    | LeafRef _ -> ()*)
    | NodeRef (n, xs) ->
        f (n, xs) acc;
        !xs |> List.iter (aux (n :: acc))
  in
  aux [] tree

(* ---------------------------------------------------------------------- *)
(* Leaf can seem redundant, but sometimes want to directly see if
 * a children is a leaf without looking if the list is empty.
 *)
type ('a, 'b) treeref2 =
  | NodeRef2 of 'a * ('a, 'b) treeref2 list ref
  | LeafRef2 of 'b

let rec (treeref_node_iter2 :
          ('a * ('a, 'b) treeref2 list ref -> unit) -> ('a, 'b) treeref2 -> unit)
    =
 fun f tree ->
  match tree with
  | LeafRef2 _ -> ()
  | NodeRef2 (n, xs) ->
      f (n, xs);
      !xs |> List.iter (treeref_node_iter2 f)

let find_treeref2 f tree =
  let res = ref [] in

  tree
  |> treeref_node_iter2 (fun (n, xs) ->
         if f (n, xs) then Stack_.push (n, xs) res);
  match !res with
  | [ (n, xs) ] -> NodeRef2 (n, xs)
  | [] -> raise Not_found
  | _x :: _y :: _zs -> raise Common.Multi_found

let (treeref_node_iter_with_parents2 :
      ('a * ('a, 'b) treeref2 list ref -> 'a list -> unit) ->
      ('a, 'b) treeref2 ->
      unit) =
 fun f tree ->
  let rec aux acc tree =
    match tree with
    | LeafRef2 _ -> ()
    | NodeRef2 (n, xs) ->
        f (n, xs) acc;
        !xs |> List.iter (aux (n :: acc))
  in
  aux [] tree

let find_treeref_with_parents_some f tree =
  let res = ref [] in

  tree
  |> treeref_node_iter_with_parents (fun (n, xs) parents ->
         match f (n, xs) parents with
         | Some v -> Stack_.push v res
         | None -> ());
  match !res with
  | [ v ] -> v
  | [] -> raise Not_found
  | _x :: _y :: _zs -> raise Common.Multi_found

let find_multi_treeref_with_parents_some f tree =
  let res = ref [] in

  tree
  |> treeref_node_iter_with_parents (fun (n, xs) parents ->
         match f (n, xs) parents with
         | Some v -> Stack_.push v res
         | None -> ());
  match !res with
  | [ _v ] -> !res
  | [] -> raise Not_found
  | _x :: _y :: _zs -> !res

(*****************************************************************************)
(* Graph. Have a look too at Ograph_*.mli  *)
(*****************************************************************************)
(*
 * Very simple implementation of a (directed) graph by list of pairs.
 * Could also use a matrix, or adjacent list, or pointer(ref).
 * todo: do some check (dont exist already, ...)
 * todo: generalise to put in common (need 'edge (and 'c ?),
 *  and take in param a display func, cos caml sux, no overloading of show :(
 *)

type 'node graph = 'node set * ('node * 'node) set

let (add_node : 'a -> 'a graph -> 'a graph) =
 fun node (nodes, arcs) -> (node :: nodes, arcs)

let (del_node : 'a -> 'a graph -> 'a graph) =
 fun node (nodes, arcs) -> (nodes $-$ set [ node ], arcs)

(* could do more job:
   let _ = assert (successors node (nodes, arcs) = empty) in
   +> List.filter (fun (src, dst) -> dst != node))
*)
let (add_arc : 'a * 'a -> 'a graph -> 'a graph) =
 fun arc (nodes, arcs) -> (nodes, set [ arc ] $+$ arcs)

let (del_arc : 'a * 'a -> 'a graph -> 'a graph) =
 fun arc (nodes, arcs) -> (nodes, arcs |> List.filter (fun a -> not (arc =*= a)))

let (successors : 'a -> 'a graph -> 'a set) =
 fun x (_nodes, arcs) ->
  arcs |> List.filter (fun (src, _dst) -> src =*= x) |> List_.map snd

let (predecessors : 'a -> 'a graph -> 'a set) =
 fun x (_nodes, arcs) ->
  arcs |> List.filter (fun (_src, dst) -> dst =*= x) |> List_.map fst

let (nodes : 'a graph -> 'a set) = fun (nodes, _arcs) -> nodes

(* pre: no cycle *)
let rec (fold_upward : ('b -> 'a -> 'b) -> 'a set -> 'b -> 'a graph -> 'b) =
 fun f xs acc graph ->
  match xs with
  | [] -> acc
  | x :: xs ->
      ( f acc x |> fun newacc ->
        fold_upward f (graph |> predecessors x) newacc graph )
      |> fun newacc -> fold_upward f xs newacc graph
(* TODO avoid already visited *)

let empty_graph = ([], [])

(*
let (add_arcs_toward: int -> (int list) -> 'a graph -> 'a graph) = fun i xs ->
  function
    (nodes, arcs) -> (nodes, (List.map (fun j -> (j,i) ) xs)++arcs)
let (del_arcs_toward: int -> (int list) -> 'a graph -> 'a graph)= fun i xs g ->
    List.fold_left (fun acc el -> del_arc (el, i) acc) g xs
let (add_arcs_from: int -> (int list) -> 'a graph -> 'a graph) = fun i xs ->
 function
    (nodes, arcs) -> (nodes, (List.map (fun j -> (i,j) ) xs)++arcs)


let (del_node: (int * 'node) -> 'node graph -> 'node graph) = fun node ->
 function (nodes, arcs) ->
  let newnodes = List.filter (fun a -> not (node = a)) nodes in
    if newnodes = nodes then (raise Not_found) else (newnodes, arcs)
let (replace_node: int -> 'node -> 'node graph -> 'node graph) = fun i n ->
 function (nodes, arcs) ->
  let newnodes = List.filter (fun (j,_) -> not (i = j)) nodes in
    ((i,n)::newnodes, arcs)
let (get_node: int -> 'node graph -> 'node) = fun i -> function
    (nodes, arcs) -> List.assoc i nodes

let (get_free: 'a graph -> int) = function
    (nodes, arcs) -> (maximum (List.map fst nodes))+1
(* require no cycle !!
  TODO if cycle check that we have already visited a node *)
let rec (succ_all: int -> 'a graph -> (int list)) = fun i -> function
    (nodes, arcs) as g ->
      let direct = succ i g in
      union direct (union_list (List.map (fun i -> succ_all i g) direct))
let rec (pred_all: int -> 'a graph -> (int list)) = fun i -> function
    (nodes, arcs) as g ->
      let direct = pred i g in
      union direct (union_list (List.map (fun i -> pred_all i g) direct))
(* require that the nodes are different !! *)
let rec (equal: 'a graph -> 'a graph -> bool) = fun g1 g2 ->
  let ((nodes1, arcs1),(nodes2, arcs2)) = (g1,g2) in
  try
   (* do 2 things, check same length and to assoc *)
    let conv = assoc_map nodes1 nodes2 in
    List.for_all (fun (i1,i2) ->
       List.mem (List.assoc i1 conv, List.assoc i2 conv) arcs2)
     arcs1
      && (List.length arcs1 = List.length arcs2)
    (* could think that only forall is needed, but need check same lenth too*)
  with _ -> false

let (display: 'a graph -> ('a -> unit) -> unit) = fun g display_func ->
  let rec aux depth i =
    print_n depth " ";
    print_int i; print_string "->"; display_func (get_node i g);
    print_string "\n";
    List.iter (aux (depth+2)) (succ i g)
  in aux 0 1

let (display_dot: 'a graph -> ('a -> string) -> unit)= fun (nodes,arcs) func ->
  let file = open_out_bin "test.dot" in
  output_string file "digraph misc {\n" ;
  List.iter (fun (n, node) ->
    output_int file n; output_string file " [label=\"";
    output_string file (func node); output_string file " \"];\n"; ) nodes;
  List.iter (fun (i1,i2) ->  output_int file i1 ; output_string file " -> " ;
    output_int file i2 ; output_string file " ;\n"; ) arcs;
  output_string file "}\n" ;
  close_out file;
  let status = Unix.system "viewdot test.dot" in
  ()
(* todo: faire = graphe (int can change !!! => cant make simply =)
   reassign number first !!
 *)

(* todo: mettre diff(modulo = !!) en rouge *)
let (display_dot2: 'a graph -> 'a graph -> ('a -> string) -> unit) =
  fun (nodes1, arcs1) (nodes2, arcs2) func ->
  let file = open_out_bin "test.dot" in
  output_string file "digraph misc {\n" ;
  output_string file "rotate = 90;\n";
  List.iter (fun (n, node) ->
    output_string file "100"; output_int file n;
    output_string file " [label=\"";
    output_string file (func node); output_string file " \"];\n"; ) nodes1;
  List.iter (fun (n, node) ->
    output_string file "200"; output_int file n;
    output_string file " [label=\"";
    output_string file (func node); output_string file " \"];\n"; ) nodes2;
  List.iter (fun (i1,i2) ->
    output_string file "100"; output_int file i1 ; output_string file " -> " ;
    output_string file "100"; output_int file i2 ; output_string file " ;\n";
    )
   arcs1;
  List.iter (fun (i1,i2) ->
    output_string file "200"; output_int file i1 ; output_string file " -> " ;
    output_string file "200"; output_int file i2 ; output_string file " ;\n"; )
   arcs2;
(*  output_string file "500 -> 1001; 500 -> 2001}\n" ; *)
  output_string file "}\n" ;
  close_out file;
  let status = Unix.system "viewdot test.dot" in
  ()


 *)
(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)
(* overloading *)

let map = List_.map (* note: really really slow, use rev_map if possible *)
let filter = List.filter
let fold = List.fold_left
let member = List.mem
let iter = List.iter
let find = List.find
let exists = List.exists
let forall = List.for_all
let big_union f xs = xs |> map f |> fold union_set empty_set

(* let empty = [] *)
let empty_list = []
let sort xs = List.sort compare xs
let length = List.length

(* in prelude now: let null xs = match xs with [] -> true | _ -> false *)
let head xs = List_.hd_exn "unexpected empty list" xs
let tail xs = List_.tl_exn "unexpected empty list" xs
let is_singleton xs = List.length xs =|= 1
(*x: common.ml *)

(*###########################################################################*)
(* Misc functions *)
(*###########################################################################*)

(*****************************************************************************)
(* Geometry (raytracer) *)
(*****************************************************************************)

type vector = float * float * float
type point = vector
type color = vector (* color(0-1) *)

(* todo: factorise *)
let (dotproduct : vector * vector -> float) =
 fun ((x1, y1, z1), (x2, y2, z2)) -> (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let (vector_length : vector -> float) =
 fun (x, y, z) -> sqrt (square x +. square y +. square z)

let (minus_point : point * point -> vector) =
 fun ((x1, y1, z1), (x2, y2, z2)) -> (x1 -. x2, y1 -. y2, z1 -. z2)

let (distance : point * point -> float) =
 fun (x1, x2) -> vector_length (minus_point (x2, x1))

let (normalise : vector -> vector) =
 fun (x, y, z) ->
  let len = vector_length (x, y, z) in
  (x /. len, y /. len, z /. len)

let (mult_coeff : vector -> float -> vector) =
 fun (x, y, z) c -> (x *. c, y *. c, z *. c)

let (add_vector : vector -> vector -> vector) =
 fun v1 v2 ->
  let (x1, y1, z1), (x2, y2, z2) = (v1, v2) in
  (x1 +. x2, y1 +. y2, z1 +. z2)

let (mult_vector : vector -> vector -> vector) =
 fun v1 v2 ->
  let (x1, y1, z1), (x2, y2, z2) = (v1, v2) in
  (x1 *. x2, y1 *. y2, z1 *. z2)

let sum_vector = List.fold_left add_vector (0.0, 0.0, 0.0)

(*****************************************************************************)
(* Pics (raytracer) *)
(*****************************************************************************)

type pixel = int * int * int (* RGB *)

(* required pixel list in row major order, line after line *)
let (write_ppm : int -> int -> pixel list -> string -> unit) =
 fun width height xs filename ->
  let chan = UStdlib.open_out_bin filename in
  output_string chan "P6\n";
  output_string chan (string_of_int width ^ "\n");
  output_string chan (string_of_int height ^ "\n");
  output_string chan "255\n";
  List.iter
    (fun (r, g, b) -> List.iter (fun byt -> output_byte chan byt) [ r; g; b ])
    xs;
  close_out chan

let test_ppm1 () =
  write_ppm 100 100
    (generate (50 * 100) (1, 45, 100) @ generate (50 * 100) (1, 1, 100))
    "img.ppm"

(*****************************************************************************)
(* Parsers (aop-colcombet) *)
(*****************************************************************************)

let parserCommon lexbuf parserer lexer =
  try
    let result = parserer lexer lexbuf in
    result
  with
  | UParsing.Parse_error ->
      UStdlib.print_string "buf: ";
      UStdlib.print_bytes lexbuf.Lexing.lex_buffer;
      UStdlib.print_string "\n";
      UStdlib.print_string "current: ";
      UStdlib.print_int lexbuf.Lexing.lex_curr_pos;
      UStdlib.print_string "\n";
      raise UParsing.Parse_error

(* marche pas ca neuneu *)
(*
let getDoubleParser parserer lexer string =
  let lexbuf1 = Lexing.from_string string in
  let chan = open_in_bin string in
  let lexbuf2 = Lexing.from_channel chan in
  (parserCommon lexbuf1 parserer lexer  , parserCommon lexbuf2 parserer lexer )
*)

let getDoubleParser parserer lexer =
  ( (function
    | string ->
        let lexbuf1 = Lexing.from_string string in
        parserCommon lexbuf1 parserer lexer),
    function
    | string ->
        let chan = UStdlib.open_in_bin string in
        let lexbuf2 = Lexing.from_channel chan in
        parserCommon lexbuf2 parserer lexer )

(*****************************************************************************)
(* parser combinators *)
(*****************************************************************************)

(* cf parser_combinators.ml
 *
 * Could also use ocaml stream. but not backtrack and forced to do LL,
 * so combinators are better.
 *
 *)

(*****************************************************************************)
(* Parser related (cocci) *)
(*****************************************************************************)
(* now in lib_parsing/parse_info.ml *)

(*x: common.ml *)
(*****************************************************************************)
(* Regression testing bis (cocci) *)
(*****************************************************************************)

(* todo: keep also size of file, compute md5sum ? cos maybe the file
 * has changed!.
 *
 * todo: could also compute the date, or some version info of the program,
 * can record the first date when was found a OK, the last date where
 * was ok, and then first date when found fail. So the
 * Common.Ok would have more information that would be passed
 * to the Common.Pb of date * date * date * string   peut etre.
 *
 * todo? maybe use plain text file instead of marshalling.
 *)

type score_result = Ok | Pb of string

(* with sexp *)
type score = (string (* usually a filename *), score_result) Hashtbl.t

(* with sexp *)
type score_list = (string (* usually a filename *) * score_result) list
(* with sexp *)

let empty_score () : score = Hashtbl.create 101

let regression_testing_vs newscore bestscore =
  let newbestscore = empty_score () in

  let allres =
    hash_to_list newscore |> List_.map fst
    $+$ (hash_to_list bestscore |> List_.map fst)
  in
  allres
  |> List.iter (fun res ->
         match
           ( optionise (fun () -> Hashtbl.find newscore res),
             optionise (fun () -> Hashtbl.find bestscore res) )
         with
         | None, None -> raise Common.Impossible
         | Some x, None ->
             UPrintf.printf "new test file appeared: %s\n" res;
             Hashtbl.add newbestscore res x
         | None, Some _x -> UPrintf.printf "old test file disappeared: %s\n" res
         | Some newone, Some bestone -> (
             match (newone, bestone) with
             | Ok, Ok -> Hashtbl.add newbestscore res Ok
             | Pb x, Ok ->
                 UPrintf.printf
                   "PBBBBBBBB: a test file does not work anymore!!! : %s\n" res;
                 UPrintf.printf "Error : %s\n" x;
                 Hashtbl.add newbestscore res Ok
             | Ok, Pb _x ->
                 UPrintf.printf "Great: a test file now works: %s\n" res;
                 Hashtbl.add newbestscore res Ok
             | Pb x, Pb y ->
                 Hashtbl.add newbestscore res (Pb x);
                 if not (x = y) then (
                   UPrintf.printf
                     "Semipb: still error but not same error : %s\n" res;
                   UPrintf.printf "%s\n" (chop ("Old error: " ^ y));
                   UPrintf.printf "New error: %s\n" x)));
  flush UStdlib.stdout;
  flush UStdlib.stderr;
  newbestscore

let get_value filename =
  let chan = UStdlib.open_in_bin filename in
  let x = UStdlib.input_value chan in
  (* <=> Marshal.from_channel  *)
  close_in chan;
  x

let write_value valu filename =
  let chan = UStdlib.open_out_bin filename in
  UStdlib.output_value chan valu;
  (* <=> Marshal.to_channel *)
  (* Marshal.to_channel chan valu [Marshal.Closures]; *)
  close_out chan

let regression_testing newscore best_score_file =
  pr2 ("regression file: " ^ best_score_file);
  let (bestscore : score) =
    if not (USys.file_exists best_score_file) then
      write_value (empty_score ()) best_score_file;
    get_value best_score_file
  in
  let newbestscore = regression_testing_vs newscore bestscore in
  write_value newbestscore (best_score_file ^ ".old");
  write_value newbestscore best_score_file;
  ()

let string_of_score_result v =
  match v with
  | Ok -> "Ok"
  | Pb s -> "Pb: " ^ s

let total_scores score =
  let total = hash_to_list score |> List.length in
  let good =
    hash_to_list score |> List.filter (fun (_s, v) -> v =*= Ok) |> List.length
  in
  (good, total)

let print_total_score score =
  pr2 "--------------------------------";
  pr2 "total score";
  pr2 "--------------------------------";
  let good, total = total_scores score in
  pr2 (Printf.sprintf "good = %d/%d" good total)

let print_score score =
  score |> hash_to_list
  |> List.iter (fun (k, v) ->
         pr2 (Printf.sprintf "%s --> %s" k (string_of_score_result v)));
  print_total_score score;
  ()
(*x: common.ml *)
(*****************************************************************************)
(* Scope managment (cocci) *)
(*****************************************************************************)

(* could also make a function Common.make_scope_functions that return
 * the new_scope, del_scope, do_in_scope, add_env. Kind of functor :)
 *)

type ('a, 'b) scoped_env = ('a, 'b) assoc list

(*
let rec lookup_env f env =
  match env with
  | [] -> raise Not_found
  | []::zs -> lookup_env f zs
  | (x::xs)::zs ->
      match f x with
      | None -> lookup_env f (xs::zs)
      | Some y -> y

let member_env_key k env =
  try
    let _ = lookup_env (fun (k',v) -> if k = k' then Some v else None) env in
    true
  with Not_found -> false

*)

let rec lookup_env k env =
  match env with
  | [] -> raise Not_found
  | [] :: zs -> lookup_env k zs
  | ((k', v) :: xs) :: zs -> if k =*= k' then v else lookup_env k (xs :: zs)

let member_env_key k env =
  match optionise (fun () -> lookup_env k env) with
  | None -> false
  | Some _ -> true

let new_scope scoped_env = scoped_env := [] :: !scoped_env

let del_scope scoped_env =
  scoped_env := List_.tl_exn "unexpected empty list" !scoped_env

let do_in_new_scope scoped_env f =
  new_scope scoped_env;
  let res = f () in
  del_scope scoped_env;
  res

let add_in_scope scoped_env def =
  let current, older = uncons !scoped_env in
  scoped_env := (def :: current) :: older

(* note that ocaml hashtbl store also old value of a binding when add
 * add a newbinding; that's why del_scope works
 *)

type ('a, 'b) scoped_h_env = {
  scoped_h : ('a, 'b) Hashtbl.t;
  scoped_list : ('a, 'b) assoc list;
}

let empty_scoped_h_env () =
  { scoped_h = Hashtbl.create 101; scoped_list = [ [] ] }

let clone_scoped_h_env x =
  { scoped_h = Hashtbl.copy x.scoped_h; scoped_list = x.scoped_list }

let lookup_h_env k env = Hashtbl.find env.scoped_h k

let member_h_env_key k env =
  match optionise (fun () -> lookup_h_env k env) with
  | None -> false
  | Some _ -> true

let new_scope_h scoped_env =
  scoped_env := { !scoped_env with scoped_list = [] :: !scoped_env.scoped_list }

let del_scope_h scoped_env =
  List_.hd_exn "unexpected empty list" !scoped_env.scoped_list
  |> List.iter (fun (k, _v) -> Hashtbl.remove !scoped_env.scoped_h k);
  scoped_env :=
    {
      !scoped_env with
      scoped_list = List_.tl_exn "unexpected empty list" !scoped_env.scoped_list;
    }

let do_in_new_scope_h scoped_env f =
  new_scope_h scoped_env;
  let res = f () in
  del_scope_h scoped_env;
  res

(*
let add_in_scope scoped_env def =
  let (current, older) = uncons !scoped_env in
  scoped_env := (def::current)::older
*)

let add_in_scope_h x (k, v) =
  Hashtbl.add !x.scoped_h k v;
  x :=
    {
      !x with
      scoped_list =
        ((k, v) :: List_.hd_exn "unexpected empty list" !x.scoped_list)
        :: List_.tl_exn "unexpected empty list" !x.scoped_list;
    }

(*****************************************************************************)
(* Terminal *)
(*****************************************************************************)

(* See console.ml *)

(*****************************************************************************)
(* Gc optimisation (pfff) *)
(*****************************************************************************)

(* opti: to avoid stressing the GC with a huge graph, we sometimes
 * change a big AST into a string, which reduces the size of the graph
 * to explore when garbage collecting.
 *)
type 'a cached = 'a serialized_maybe ref
and 'a serialized_maybe = Serial of string | Unfold of 'a

let serial x = ref (Serial (Marshal.to_string x []))

let unserial x =
  match !x with
  | Unfold c -> c
  | Serial s ->
      let res = UMarshal.from_string s 0 in
      (*        x := Unfold res; *)
      res

(*****************************************************************************)
(* Random *)
(*****************************************************************************)

(* let _init_random = Random.self_init () *)

(*
let random_insert i l =
    let p = Random.int (length l +1)
    in let rec insert i p l =
      if (p = 0) then i::l else (hd l)::insert i (p-1) (tl l)
    in insert i p l

let rec randomize_list = function
  []  -> []
  | a::l -> random_insert a (randomize_list l)
*)
let random_list xs = List.nth xs (URandom.int (length xs))

(* todo_opti: use fisher/yates algorithm.
 * ref: http://en.wikipedia.org/wiki/Knuth_shuffle
 *
 * public static void shuffle (int[] array)
 * {
 *  Random rng = new Random ();
 *  int n = array.length;
 *  while (--n > 0)
 *  {
 *    int k = rng.nextInt(n + 1);  // 0 <= k <= n (!)
 *    int temp = array[n];
 *    array[n] = array[k];
 *    array[k] = temp;
 *   }
 * }

 *)
let randomize_list xs =
  let permut = permutation xs in
  random_list permut

let random_subset_of_list num xs =
  let array = Array.of_list xs in
  let len = Array.length array in

  let h = Hashtbl.create 101 in
  let cnt = ref num in
  while !cnt > 0 do
    let x = URandom.int len in
    if not (Hashtbl.mem h array.(x)) (* bugfix2: not just x :) *) then (
      Hashtbl.add h array.(x) true;
      (* bugfix1: not just x :) *)
      decr cnt)
  done;
  let objs = hash_to_list h |> List_.map fst in
  objs

(*x: common.ml *)
(*###########################################################################*)
(* Postlude *)
(*###########################################################################*)

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)

(*s: common.ml cmdline *)

(* I put it inside a func as it can help to give a chance to
 * change the globals before getting the options as some
 * options sometimes may want to show the default value.
 *)
let cmdline_flags_devel () =
  [
    ( "-debugger",
      Arg.Set Common.debugger,
      " option to set if launched inside ocamldebug" );
  ]

let cmdline_flags_verbose () =
  [
    ("-verbose_level", Arg.Set_int verbose_level, " <int> guess what");
    ( "-disable_pr2_once",
      Arg.Set UCommon.disable_pr2_once,
      " to print more messages" );
  ]

let cmdline_flags_other () =
  [
    ("-nocheck_stack", Arg.Clear _check_stack, " ");
    ("-batch_mode", Arg.Set _batch_mode, " no interactivity");
  ]

(* potentially other common options but not yet integrated:

   "-timeout",        Arg.Set_int timeout,
   "  <sec> interrupt LFS or buggy external plugins";

   (* can't be factorized because of the $ cvs stuff, we want the date
   * of the main.ml file, not common.ml
   *)
   "-version",   Arg.Unit (fun () ->
    pr2 "version: _dollar_Date: 2008/06/14 00:54:22 _dollar_";
    raise (Common.UnixExit 0)
    ),
   "   guess what";

   "-shorthelp", Arg.Unit (fun () ->
    !short_usage_func();
    raise (Common.UnixExit 0)
   ),
   "    see short list of options";
   "-longhelp", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
    ),
   "-help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
   ),
   " ";
   "--help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
   ),
   " ";
*)

let cmdline_actions () =
  [
    ( "-test_check_stack",
      "  <limit>",
      Arg_.mk_action_1_arg test_check_stack_size );
  ]

(*e: common.ml cmdline *)

(*x: common.ml *)
(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)
(* stuff put here cos of of forward definition limitation of ocaml *)

(* Infix trick, seen in jane street lib and harrop's code, and maybe in GMP *)
module Infix = struct
  let ( ==~ ) = ( ==~ )
end

let with_pr2_to_string caps f =
  CapTmp.with_temp_file caps ~suffix:".out" (fun path ->
      let file = Fpath.to_string path in
      redirect_stdout_stderr file f;
      cat file)

(*---------------------------------------------------------------------------*)
(* Directories part 2 *)
(*---------------------------------------------------------------------------*)

(* todo? vs common_prefix_of_files_or_dirs? *)
let find_common_root files =
  let dirs_part = files |> List_.map fst in

  let rec aux current_candidate xs =
    try
      let topsubdirs =
        xs |> List_.map (List_.hd_exn "unexpected empty list") |> uniq_eff
      in
      match topsubdirs with
      | [ x ] ->
          aux (x :: current_candidate)
            (xs |> List_.map (List_.tl_exn "unexpected empty list"))
      | _ -> List.rev current_candidate
    with
    | _ -> List.rev current_candidate
  in
  aux [] dirs_part

(*
let _ = example
  (find_common_root
      [(["home";"pad"], "foo.php");
       (["home";"pad";"bar"], "bar.php");
      ]
    =*= ["home";"pad"])
*)

let dirs_and_base_of_file file =
  let dir, base = Filename_.db_of_filename file in
  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  (dirs, base)

(*
let _ = example
  (dirs_and_base_of_file "/home/pad/foo.php" =*= (["home";"pad"], "foo.php"))
*)

let inits_of_absolute_dir dir =
  if not (is_absolute dir) then
    failwith (spf "inits_of_absolute_dir: %s is not an absolute path" dir);
  if not (UFile.is_dir ~follow_symlinks:true (Fpath.v dir)) then
    failwith (spf "inits_of_absolute_dir: %s is not a directory" dir);
  let dir = chop_dirsymbol dir in

  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  inits dirs |> List_.map (fun xs -> "/" ^ join "/" xs)

let inits_of_relative_dir dir =
  if not (is_relative dir) then
    failwith (spf "inits_of_relative_dir: %s is not a relative dir" dir);
  let dir = chop_dirsymbol dir in

  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  inits dirs
  |> List_.tl_exn "unexpected empty list"
  |> List_.map (fun xs -> join "/" xs)

(*
let _ = example
  (inits_of_absolute_dir "/usr/bin" =*= (["/"; "/usr"; "/usr/bin"]))

let _ = example
  (inits_of_relative_dir "usr/bin" =*= (["usr"; "usr/bin"]))
*)

(* main entry *)
let (tree_of_files : filename list -> (string, string * filename) tree) =
 fun files ->
  let files_fullpath = files in

  (* extract dirs and file from file, e.g. ["home";"pad"], "__flib.php", path *)
  let files = files |> List_.map dirs_and_base_of_file in

  (* find root, eg ["home";"pad"] *)
  let root = find_common_root files in

  let files = zip files files_fullpath in

  (* remove the root part *)
  let files =
    files
    |> List_.map (fun ((dirs, base), path) ->
           let n = List.length root in
           let root', rest = (take n dirs, drop n dirs) in
           assert (root' =*= root);
           ((rest, base), path))
  in

  (* now ready to build the tree recursively *)
  let rec aux (xs : ((string list * string) * filename) list) =
    let files_here, rest =
      xs |> List.partition (fun ((dirs, _base), _) -> List_.null dirs)
    in
    let groups =
      rest
      |> group_by_mapped_key (fun ((dirs, _base), _) ->
             (* would be a file if null dirs *)
             assert (not (List_.null dirs));
             List_.hd_exn "unexpected empty list" dirs)
    in

    let nodes =
      groups
      |> List_.map (fun (k, xs) ->
             let xs' =
               xs
               |> List_.map (fun ((dirs, base), path) ->
                      ((List_.tl_exn "unexpected empty list" dirs, base), path))
             in
             Node (k, aux xs'))
    in
    let leaves =
      files_here |> List_.map (fun ((_dir, base), path) -> Leaf (base, path))
    in
    nodes @ leaves
  in
  Node (join "/" root, aux files)

(* finding the common root *)
let common_prefix_of_files_or_dirs xs =
  let xs = xs |> List_.map relative_to_absolute in
  match xs with
  | [] -> failwith "common_prefix_of_files_or_dirs: empty list"
  | [ x ] -> x
  | _y :: _ys ->
      (* todo: work when dirs ?*)
      let xs = xs |> List_.map dirs_and_base_of_file in
      let dirs = find_common_root xs in
      "/" ^ join "/" dirs

(*
let _ =
  example
    (common_prefix_of_files_or_dirs ["/home/pad/visual";
                                     "/home/pad/commons";]
     =*= "/home/pad/pfff"
    )
*)
