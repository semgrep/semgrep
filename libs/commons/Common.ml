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

let pr s =
  UStdlib.print_string s;
  UStdlib.print_string "\n";
  flush UStdlib.stdout

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
  (* nosemgrep: no-fun-protect *)
  try Fun.protect ~finally work with
  | Fun.Finally_raised exn1 as exn ->
      (* Just re-raise whatever exception was raised during a 'finally',
       * drop 'Finally_raised'.
       *)
      logger#error "protect: %s" (exn |> Exception.catch |> Exception.to_string);
      Exception.catch_and_reraise exn1

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

let pr_time name f =
  let t1 = UUnix.gettimeofday () in
  protect f ~finally:(fun () ->
      let t2 = UUnix.gettimeofday () in
      pr (spf "%s: %.6f s" name (t2 -. t1)))

let pr2_time name f =
  let t1 = UUnix.gettimeofday () in
  protect f ~finally:(fun () ->
      let t2 = UUnix.gettimeofday () in
      pr2 (spf "%s: %.6f s" name (t2 -. t1)))

(*###########################################################################*)
(* Exn *)
(*###########################################################################*)

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
(* now earlier *)

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
(* Filenames *)
(*****************************************************************************)

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
  let chan = UUnix.open_process_in command in
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
  let chan = UStdlib.open_in_bin file in
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
    let ic = UStdlib.open_in_bin path in
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
    let fd = UUnix.openfile path [ Unix.O_RDONLY ] 0 in
    protect ~finally:(fun () -> Unix.close fd) (fun () -> loop fd)

let write_file ~file s =
  let chan = UStdlib.open_out_bin file in
  output_string chan s;
  close_out chan

(* could be in control section too *)

let fullpath file =
  if not (USys.file_exists file) then
    failwith (spf "fullpath: file (or directory) %s does not exist" file);
  let dir, base =
    if USys.is_directory file then (file, None)
    else (Filename.dirname file, Some (Filename.basename file))
  in
  (* save *)
  let old = USys.getcwd () in

  USys.chdir dir;
  let here = USys.getcwd () in

  (* restore *)
  USys.chdir old;

  match base with
  | None -> here
  | Some x -> Filename.concat here x

(* emacs/lisp inspiration (eric cooper and yaron minsky use that too) *)
let (with_open_outfile :
      string (* filename *) -> ((string -> unit) * out_channel -> 'a) -> 'a) =
 fun file f ->
  let chan = UStdlib.open_out_bin file in
  let pr s = output_string chan s in
  unwind_protect
    (fun () ->
      let res = f (pr, chan) in
      close_out chan;
      res)
    (fun _e -> close_out chan)

let (with_open_infile : string (* filename *) -> (in_channel -> 'a) -> 'a) =
 fun file f ->
  let chan = UStdlib.open_in_bin file in
  unwind_protect
    (fun () ->
      let res = f chan in
      close_in chan;
      res)
    (fun _e ->
      (* TODO? use close_in_noerr? *)
      close_in chan)

(* creation of tmp files, a la gcc *)

let _temp_files_created = Hashtbl.create 101

(* ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" *)
let new_temp_file prefix suffix =
  let pid = if !jsoo then 42 else UUnix.getpid () in
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
           USys.remove s);
    Hashtbl.clear _temp_files_created)

let erase_this_temp_file f =
  if not !save_tmp_files then (
    Hashtbl.remove _temp_files_created f;
    logger#info "erasing: %s" f;
    USys.remove f)

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
  | UnixExit x -> UStdlib.exit x

let pp_do_in_zero_box f =
  UFormat.open_box 0;
  f ();
  UFormat.close_box ()

let before_exit = ref []

let main_boilerplate f =
  if not !Sys.interactive then
    exn_to_real_unixexit (fun () ->
        USys.set_signal USys.sigint
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
               USys.set_signal USys.sigint Sys.Signal_default;
               raise (UnixExit (-1))));

        (* The finalize() below makes it tedious to go back from exns when we use
         * 'back' in ocamldebug. Hence the special code in finalize() to
         * run differently when in "debugger mode". However the
         * Common.debugger global will be set in main(), so too late, so
         * we have to be quicker here and set it for the finalize() below.
         *)
        if
          USys.argv |> Array.to_list
          |> List.exists (fun x -> x = "-debugger" || x = "--debugger")
        then debugger := true;

        finalize
          (fun () ->
            pp_do_in_zero_box (fun () ->
                try f () with
                (* <---- here it is *)
                | UUnix.Unix_error (e, fm, argm) ->
                    pr2
                      (spf "exn Unix_error: %s %s %s\n" (Unix.error_message e)
                         fm argm);
                    raise (UUnix.Unix_error (e, fm, argm))))
          (fun () ->
            !before_exit |> List.iter (fun f -> f ());
            erase_temp_files ()))
(* let _ = if not !Sys.interactive then (main ()) *)

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs -> (
        match f with
        | f when not (Sys.file_exists f) ->
            logger#error "%s does not exist anymore" f;
            loop result fs
        | f when Sys.is_directory f ->
            Sys.readdir f |> Array.to_list
            |> List_.map (Filename.concat f)
            |> List.append fs |> loop result
        | f -> loop (f :: result) fs)
    | [] -> result
  in
  loop [] [ dir ]

let follow_symlinks = ref false

let vcs_re =
  "(^((\\.hg)|(CVS)|(\\.git)|(_darcs)|(\\.svn))$)|(.*\\.git_annot$)|(.*\\.marshall$)"
  |> Re.Posix.re |> Re.compile

let files_of_dir_or_files_no_vcs_nofilter xs =
  xs
  |> List_.map (fun x ->
         if Sys.is_directory x then
           let files = dir_contents x in
           List.filter (fun x -> not (Re.execp vcs_re x)) files
         else [ x ])
  |> List_.flatten

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
