(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Inspired by 'tbgs' and big_grep at facebook.
 * The trick is to build a giant string and run compiled-regexps
 * on it. For each match have to go back to find the start and
 * end of entity, or the entity number so can display
 * the information associated with it. So need markers
 * in the string.
 *
 * One-liner in perl by Erling:
 * perl -e '$|++; open F,"/usr/share/dict/words"; { local $/; $all=<F>;
 * } while(<STDIN>) { chomp; $w=$_; $n = 0; while($all =~ /$w.*/g) {
 * print "$&\n"; last if ++$n>10; } print "[$w]\n"; }'
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type index = {
  big_string: string;
  pos_to_entity: (int, Db.entity) Hashtbl.t;
  case_sensitive: bool;
}

(* using \n is convenient so can allow regexp queries like
 * employee.* without having the regexp engine to try to match
 * the whole string; it will stop at the first \n.
*)
let separation_marker_char = '\n'

let empty_index () = {
  big_string = "";
  pos_to_entity = Hashtbl.create 1;
  case_sensitive = false;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (==~) = Common2.(==~)

(*****************************************************************************)
(* Naive version *)
(*****************************************************************************)

(* This is the naive version, just to have a baseline for benchmarks *)
let naive_top_n_search2 ~top_n ~query xs =
  let re = Str.regexp (".*" ^ query) in

  let rec aux ~n xs =
    if n = top_n
    then []
    else
      (match xs with
       | [] -> []
       | e::xs ->
           if e.Db.e_name ==~ re
           then
             e::aux ~n:(n+1) xs
           else
             aux ~n xs
      )
  in
  aux ~n:0 xs


let naive_top_n_search ~top_n ~query idx =
  Common.profile_code "Big_grep.naive_top_n" (fun () ->
    naive_top_n_search2 ~top_n ~query idx
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build_index2 ?(case_sensitive=false) entities =

  let buf = Buffer.create 20_000_000 in
  let h = Hashtbl.create 1001 in

  let current = ref 0 in

  entities |> List.iter (fun e ->
    (* Use fullname ? The caller, that is for instance
     * files_and_dirs_and_sorted_entities_for_completion
     * should have done the job of putting the fullename in e_name.
    *)
    let s = Common2.string_of_char separation_marker_char ^ e.Db.e_name in
    let s =
      if case_sensitive
      then s
      else String.lowercase_ascii s
    in

    Buffer.add_string buf s;
    Hashtbl.add h !current e;
    current := !current + String.length s;
  );
  (* just to make it easier to code certain algorithms such as
   * find_position_marker_after
  *)
  Buffer.add_string buf (Common2.string_of_char separation_marker_char);

  {
    big_string = Buffer.contents buf;
    pos_to_entity = h;
    case_sensitive = case_sensitive;
  }

let build_index ?case_sensitive a =
  Common.profile_code "Big_grep.build_idx" (fun () ->
    build_index2 ?case_sensitive a)


let find_position_marker_before start_pos str =
  let pos = ref (start_pos - 1) in

  while String.get str !pos <> separation_marker_char do
    pos := !pos - 1
  done;
  !pos

let find_position_marker_after start_pos str =
  let pos = ref (start_pos + 1) in

  while String.get str !pos <> separation_marker_char do
    pos := !pos + 1
  done;
  !pos

(* the query can now contain multipe words *)
let top_n_search2 ~top_n ~query idx =

  let query =
    if idx.case_sensitive then query else String.lowercase_ascii query
  in

  let words = Str.split (Str.regexp "[ \t]+") query in
  let re =
    match words with
    | [_] -> Str.regexp (".*" ^ query)
    | [a;b] ->
        Str.regexp (spf
                      ".*\\(%s.*%s\\)\\|\\(%s.*%s\\)"
                      a b b a)
    | _ ->
        failwith "more-than-2-words query is not supported; give money to pad"
  in

  let rec aux ~n ~pos =
    if n = top_n
    then []
    else
      try
        let new_pos = Str.search_forward re idx.big_string pos in
        (* let's found the marker *)
        let pos_mark =
          find_position_marker_before new_pos idx.big_string in
        let pos_next_mark =
          find_position_marker_after new_pos idx.big_string in
        let e = Hashtbl.find idx.pos_to_entity pos_mark in
        e::aux ~n:(n+1) ~pos:pos_next_mark
      with Not_found -> []
  in
  aux ~n:0 ~pos:0


let top_n_search ~top_n ~query idx =
  Common.profile_code "Big_grep.top_n" (fun () ->
    top_n_search2 ~top_n ~query idx
  )
