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

let sum_float = List.fold_left ( +. ) 0.0
let sum_int = List.fold_left ( + ) 0

let rec drop n xs =
  match (n, xs) with
  | 0, _ -> xs
  | _, [] -> failwith "drop: not enough"
  | n, _x :: xs -> drop (n - 1) xs

let rec take n xs =
  match (n, xs) with
  | 0, _ -> []
  | _, [] -> failwith "Common.take: not enough"
  | n, x :: xs -> x :: take (n - 1) xs

let exclude p xs = List.filter (fun x -> not (p x)) xs

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

let (print_n : int -> string -> unit) =
 fun i s -> do_n i (fun () -> UStdlib.print_string s)

let (printerr_n : int -> string -> unit) =
 fun i s -> do_n i (fun () -> UStdlib.prerr_string s)

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

let internal_error s = failwith ("internal error: " ^ s)
let error_cant_have x = internal_error ("cant have this case" ^ Dumper.dump x)

(* want or of merd, but cant cos cant put die ... in b (strict call) *)
let ( ||| ) a b =
  try a with
  | _ -> b

(* emacs/lisp inspiration, (vouillon does that too in unison I think) *)

(* now in Prelude:
 * let unwind_protect f cleanup = ...
 * let finalize f cleanup =  ...
 *)

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
let string_of_chars cs = cs |> List_.map (String.make 1) |> String.concat ""

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let thd3 (_, _, z) = z

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

let string_match_substring re s =
  try
    let _i = Str.search_forward re s 0 in
    true
  with
  | Not_found -> false

let all_match re s =
  let regexp = Str.regexp re in
  let res = ref [] in
  let _ =
    Str.global_substitute regexp
      (fun _s ->
        let _ = Str.matched_string s in
        (* @Effect: also use it's side effect *)
        let paren_matched = Str.matched_group 1 s in
        Stack_.push paren_matched res;
        "" (* @Dummy *))
      s
  in
  List.rev !res

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

let chop = function
  | "" -> ""
  | s -> String.sub s 0 (String.length s - 1)

let strip c s =
  let rec remove_prefix s =
    match s with
    | [] -> []
    | c' :: cs -> if c =*= c' then remove_prefix cs else c' :: cs
  in
  list_of_string s |> remove_prefix |> List.rev |> remove_prefix |> List.rev
  |> string_of_chars

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
  Str.split_delim (Str.regexp "\n") s |> lines_aux

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

(* This regex matches the directory part a glob pattern
   used below. This way we are only trying to match
   files contained in the dir specified by the pattern or subdirs,
   instead of caluclating the contents of the entire
   working directory. I.e. tests/**/*.extension would
   result in tests/ *)
let dir_regex = Str.regexp "^[^\\*]*"

let glob pattern =
  let pattern =
    if Sys.win32 then
      (* [Re.Glob] can match a _string_ with backslashes correctly, but the
         _pattern_ tested against will not glob correctly if it contains
         backslashes as path separators. E.g., a glob pattern
         ["foo/**/*.json"] can match a Windows path ["foo\\bar\\baz.json"], but the
         pattern ["foo\\**\\*.json"] is not a usable glob pattern. Since we
         construct glob patterns from file paths, we need to ensure that those
         paths are normalized to form proper glob patterns.  *)
      Fpath.segs pattern |> String.concat "/"
    else
      Fpath.to_string pattern
  in
  Str.search_forward dir_regex pattern 0 |> ignore;
  let dir = Str.matched_string pattern in
  let regex =
    pattern
    |> Re.Glob.glob
         ~anchored:true
         (* allows globbing against windows separators *)
         ~match_backslashes:true
    |> Re.compile
  in
  (* TODO: should remove, but that would require modifying many call sites
   * so let's forge one for now
   *)
  let caps = Cap.readdir_UNSAFE () in
  let files = UFile.Legacy.dir_contents caps dir in
  files |> List_.filter_map (fun s ->
               if Re.execp regex s then
                 Some (Fpath.v s)
               else
                 None)

let unix_diff file1 file2 =
  let cmd = (Cmd.Name "diff", [ "-u"; file1; file2 ]) in
  (* nosemgrep: forbid-exec *)
  match UCmd.lines_of_run ~trim:true cmd with
  | Ok (xs, _status) -> xs
  | Error (`Msg s) -> failwith (spf "unix_diff problem: %s" s)

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

let rec inits = function
  | [] -> [ [] ]
  | e :: l -> [] :: List_.map (fun l -> e :: l) (inits l)

let rec zip xs ys =
  match (xs, ys) with
  | [], [] -> []
  | [], _ -> failwith "zip: not same length"
  | _, [] -> failwith "zip: not same length"
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let unzip zs =
  List_.fold_right (fun e (xs, ys) -> (fst e :: xs, snd e :: ys)) zs ([], [])

let unzip3 l =
  let rec unzip aa bb cc = function
    | (a, b, c) :: l -> unzip (a :: aa) (b :: bb) (c :: cc) l
    | [] -> (List.rev aa, List.rev bb, List.rev cc)
  in
  unzip [] [] [] l

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

let head_middle_tail xs =
  match xs with
  | x :: y :: xs ->
      let head = x in
      let reversed = List.rev (y :: xs) in
      let tail = List_.hd_exn "unexpected empty list" reversed in
      let middle = List.rev (List_.tl_exn "unexpected empty list" reversed) in
      (head, middle, tail)
  | _ -> failwith "head_middle_tail, too small list"

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

let maximum l = foldl1 max l
let minimum l = foldl1 min l

let rec uniq = function
  | [] -> []
  | e :: l -> if List.mem e l then uniq l else e :: uniq l

let rec splitAt n xs =
  if n =|= 0 then ([], xs)
  else
    match xs with
    | [] -> ([], [])
    | x :: xs ->
        let a, b = splitAt (n - 1) xs in
        (x :: a, b)

let map_flatten f l =
  let rec map_flatten_aux accu = function
    | [] -> accu
    | e :: l -> map_flatten_aux (List.rev (f e) @ accu) l
  in
  List.rev (map_flatten_aux [] l)

(*****************************************************************************)
(* Set. Have a look too at set*.mli  *)
(*****************************************************************************)

type 'a set = 'a list

let (insert_set : 'a -> 'a set -> 'a set) =
 fun x xs -> if List.mem x xs then xs else x :: xs

let (union_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 ->
  s2
  |> List.fold_left
       (fun acc x -> if List.mem x s1 then acc else insert_set x acc)
       s1

let ( $+$ ) = union_set

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

let group_assoc_bykey_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl_.push h k v);
  let keys = Hashtbl_.hkeys h in
  keys |> List_.map (fun k -> (k, Hashtbl_.get_stack h k))

let uniq_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun k -> Hashtbl.replace h k true);
  Hashtbl_.hkeys h

let diff_set_eff xs1 xs2 =
  let h1 = Hashtbl_.hashset_of_list xs1 in
  let h2 = Hashtbl_.hashset_of_list xs2 in
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
  ( Hashtbl_.hashset_to_list hcommon,
    Hashtbl_.hashset_to_list honly_in_h1,
    Hashtbl_.hashset_to_list honly_in_h2 )

(*****************************************************************************)
(* N-ary tree *)
(*****************************************************************************)

type ('a, 'b) tree = Node of 'a * ('a, 'b) tree list | Leaf of 'b
(* with tarzan *)

(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)
(* overloading *)

let map = List_.map

(*###########################################################################*)
(* Misc functions *)
(*###########################################################################*)

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
    Hashtbl_.hash_to_list newscore
    |> List_.map fst
    $+$ (Hashtbl_.hash_to_list bestscore |> List_.map fst)
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
  let total = Hashtbl_.hash_to_list score |> List.length in
  let good =
    Hashtbl_.hash_to_list score
    |> List.filter (fun (_s, v) -> v =*= Ok)
    |> List.length
  in
  (good, total)

let print_total_score score =
  pr2 "--------------------------------";
  pr2 "total score";
  pr2 "--------------------------------";
  let good, total = total_scores score in
  pr2 (Printf.sprintf "good = %d/%d" good total)

let print_score score =
  score |> Hashtbl_.hash_to_list
  |> List.iter (fun (k, v) ->
         pr2 (Printf.sprintf "%s --> %s" k (string_of_score_result v)));
  print_total_score score;
  ()

(*x: common.ml *)
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
let random_list xs = List.nth xs (URandom.int (List.length xs))

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
  let objs = Hashtbl_.hash_to_list h |> List_.map fst in
  objs

(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)
(* stuff put here cos of of forward definition limitation of ocaml *)

(*---------------------------------------------------------------------------*)
(* Directories part 2 *)
(*---------------------------------------------------------------------------*)

let relative_path_of_segs (segs : string list) : Fpath.t =
  match segs with
  | [] -> invalid_arg (__FUNCTION__ ^ ": empty list of segments")
  | x :: xs -> List.fold_left Fpath.add_seg (Fpath.v x) xs

let inits_of_relative_dir dir =
  if not (Fpath.is_rel dir) then
    failwith (spf "inits_of_relative_dir: %s is not a relative dir"
                (Fpath.to_string dir));
  (* Remove any trailing seperators, e.g., `a/b/c///` -> `a/b/c` *)
  let dir = Fpath.rem_empty_seg dir in
  let dirs = Fpath.segs dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  inits dirs
  |> List_.tl_exn "unexpected empty list"
  |> List_.map relative_path_of_segs

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

let dirs_and_base_of_file file =
  let dir, base = Filename_.db_of_filename file in
  let dirs = String.split_on_char '/' dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  (dirs, base)

(* main entry *)
let (tree_of_files : string list -> (string, string * string) tree) =
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
  let rec aux (xs : ((string list * string) * string) list) =
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
  Node (String.concat "/" root, aux files)
