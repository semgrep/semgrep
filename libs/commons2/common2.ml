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

let rec (foldn : ('a -> int -> 'a) -> 'a -> int -> 'a) =
 fun f acc i -> if i =|= 0 then acc else foldn f (f acc i) (i - 1)

let sum_float = List.fold_left ( +. ) 0.0
let sum_int = List.fold_left ( + ) 0

(*let last l = List_.hd_exn "unexpected empty list" (last_n 1 l) *)
let rec list_last = function
  | [] -> raise Not_found
  | [ x ] -> x
  | _x :: y :: xs -> list_last (y :: xs)

let (list_of_string : string -> char list) = function
  | "" -> []
  | s -> List_.enum 0 (String.length s - 1) |> List_.map (String.get s)

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
(* Used only in Common2
 * TODO: remove it's uses in this file.
 * NOTE: I am pretty sure this function was broken before, as it extensivly used
 * two references that don't seem to be updated, and in the default case result
 * in a no-op.
 *)
let pr2 s =
  UStdlib.prerr_string " ";
  UStdlib.prerr_string s;
  UStdlib.prerr_string "\n";
  flush UStdlib.stderr;
  ()

let spf = Printf.sprintf

(*****************************************************************************)
(* String_of *)
(*****************************************************************************)

let string_of_list f xs = "[" ^ (xs |> List_.map f |> String.concat ";") ^ "]"

let string_of_option f = function
  | None -> "None "
  | Some x -> "Some " ^ f x

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)

let internal_error s = failwith ("internal error: " ^ s)
let error_cant_have x = internal_error ("cant have this case" ^ Dumper.dump x)

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
         paths are normalized to form proper glob patterns. *)
      Fpath.segs pattern |> String.concat "/"
    else Fpath.to_string pattern
  in
  Str.search_forward dir_regex pattern 0 |> ignore;
  let dir = Str.matched_string pattern in
  let regex =
    pattern
    |> Re.Glob.glob
         ~anchored:true (* allows globbing against windows separators *)
         ~match_backslashes:true
    |> Re.compile
  in
  (* TODO: should remove, but that would require modifying many call sites
   * so let's forge one for now
   *)
  let caps = Cap.readdir_UNSAFE () in
  let files = UFile.Legacy.dir_contents caps dir in
  files
  |> List_.filter_map (fun s ->
         if Re.execp regex s then Some (Fpath.v s) else None)

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

let group_assoc_bykey_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl_.push h k v);
  let keys = Hashtbl_.hkeys h in
  keys |> List_.map (fun k -> (k, Hashtbl_.get_stack h k))

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
           (Hashtbl.find_opt newscore res, Hashtbl.find_opt bestscore res)
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
    if not (Sys_.file_exists best_score_file) then
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

(*---------------------------------------------------------------------------*)
(* Directories part 2 *)
(*---------------------------------------------------------------------------*)

let relative_path_of_segs (segs : string list) : Fpath.t =
  match segs with
  | [] -> invalid_arg (__FUNCTION__ ^ ": empty list of segments")
  | x :: xs -> List.fold_left Fpath.add_seg (Fpath.v x) xs

let inits_of_relative_dir dir =
  if not (Fpath.is_rel dir) then
    failwith
      (spf "inits_of_relative_dir: %s is not a relative dir"
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

let dirs_and_base_of_file file =
  let dir, base = Filename_.db_of_filename file in
  let dirs = String.split_on_char '/' dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  (dirs, base)
