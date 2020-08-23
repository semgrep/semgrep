(*
   Match a parsed pattern against a parsed document.

   Expectations:

   1. A flat (non-indented) pattern may match both a flat and an indented
      document.
   2. An indented pattern may only match an indented document.

   i.e. if the user bothers indenting their pattern, we honor this constraint.

   Example
   -------

   pattern:

     f(x) { a; }

   matching document:

     f(x) { a; }

   other matching document:

     f(x) {
       a;
     }

   other matching document:

     f(x)
     {
       a; }

*)

open Printf
open Doc_AST
open Pattern_AST

let debug = ref false

(* Map from metavariables to their captured value, which is a Word. *)
module Env = Map.Make (String)
type env = string Env.t

type match_result =
  | Complete of env * Loc.t
  | Fail

(* Continuation that matches the pattern against the empty document.
   To be used as the last argument of the 'match_' function. *)
let rec full_match ~dots env last_loc pat =
  match pat with
  | [] -> Complete (env, last_loc)
  | Dots _ :: pat -> full_match ~dots env last_loc pat
  | [End] -> Complete (env, last_loc)
  | End :: _ -> assert false
  | _ -> Fail

let loc_lnum (loc : Loc.t) =
  let pos, _ = loc in
  pos.pos_lnum

(*
   Find the rightmost location in a document and return it only if it's
   not too far (past the maximum line max_line_num).
 *)
let rec extend_last_loc ~max_line_num last_loc (doc : Doc_AST.node list) =
  match doc with
  | [] -> Some last_loc
  | List doc1 :: doc2 ->
      (match extend_last_loc ~max_line_num last_loc doc1 with
       | None -> None
       | Some last_loc ->
           extend_last_loc ~max_line_num last_loc doc2
      )
  | Atom (loc, _) :: doc ->
      if loc_lnum loc <= max_line_num then
        extend_last_loc ~max_line_num loc doc
      else
        None

let doc_matches_dots ~dots last_loc doc =
  match dots, doc with
  | None, [] -> Some last_loc
  | None, _ -> None
  | Some max_line_num, doc ->
      if loc_lnum last_loc <= max_line_num then
        extend_last_loc ~max_line_num last_loc doc
      else
        None

let loc_matches_dots ~dots loc =
  match dots with
  | None -> false
  | Some max_line_num ->
      loc_lnum loc <= max_line_num

(*
   Match a pattern against a document tree.

   dots = Some max_line_num
          indicates we're allowed to skip the first document node if it doesn't
          match the pattern. The line number of the skipped node may not
          be greater than the specified value.
   cont = call to match the rest of the document against the rest of the
          pattern when reaching the end of the current sub-document.
*)
let rec match_
    ~(dots:int option)
    (env : env)
    (last_loc : Loc.t)
    (pat : Pattern_AST.node list )
    (doc : Doc_AST.node list)
    (cont : (dots:int option ->
             env -> Loc.t -> Pattern_AST.node list -> match_result))
  : match_result =
  if !debug then
    Print_match.print pat doc;
  match pat, doc with
  | [], doc ->
      (match doc_matches_dots ~dots last_loc doc with
       | Some last_loc ->
           Complete (env, last_loc)
       | None ->
           Fail
      )
  | [End], doc ->
      (match doc_matches_dots ~dots last_loc doc with
       | Some last_loc ->
           Complete (env, last_loc)
       | None ->
           Complete (env, last_loc)
      )
  | End :: _, _ -> assert false

  | List pat1 :: pat2, doc ->
      (match doc with
       | [] ->
           (* Nothing left to match against. *)
           assert (pat1 <> []);
           Fail
       | List doc1 :: doc2 ->
           (* Indented block coincides with an indented block in the document.
              These blocks must match, independently from the rest. *)
           (match match_ ~dots:None env last_loc pat1 doc1 full_match with
            | Complete (env, last_loc) ->
                match_ ~dots:None env last_loc pat2 doc2 cont
            | Fail -> Fail
           )
       | Atom (loc, _) :: doc_tail ->
           (* Indented block in pattern doesn't match in the document.
              Skip document node if allowed. *)
           assert (pat1 <> []);
           if loc_matches_dots ~dots loc then
             match_ ~dots env last_loc pat doc_tail cont
           else
             Fail
      )

  | Dots _ :: pat_tail, doc ->
      let dots =
        (* allow '...' to extend for at most 10 lines *)
        let _, last_pos = last_loc in
        Some (last_pos.pos_lnum + 10)
      in
      match_ ~dots env last_loc pat_tail doc cont

  | Atom (_, p) :: pat_tail, doc ->
      match doc with
      | [] -> cont ~dots env last_loc pat
      | doc_head :: doc_tail ->
          match doc_head with
          | List sub_doc ->
              (* Indented block in the document doesn't have to match
                 indented block in the pattern. We just continue matching
                 in the block as if the document was flat. *)
              match_ ~dots env last_loc pat sub_doc
                (fun ~dots env last_loc pat ->
                   (* The sub-block was matched but some of the pattern wasn't
                      consumed. We continue, in the sub-block's parent. *)
                   match_ ~dots env last_loc pat doc_tail cont
              )
          | Atom (loc, d) ->
              let match_result =
                match p, d with
                | Metavar name, Word value ->
                    (match Env.find_opt name env with
                     | None ->
                         (* First encounter of the metavariable,
                            store its value. *)
                         let env = Env.add name value env in
                         match_ ~dots:None env loc pat_tail doc_tail cont
                     | Some value0 ->
                         (* Check if value matches previously captured
                            value. *)
                         if value = value0 then
                           match_ ~dots:None env loc pat_tail doc_tail cont
                         else
                           Fail
                    )
                | Word a, Word b when a = b ->
                    match_ ~dots:None env loc pat_tail doc_tail cont
                | Punct a, Punct b when a = b ->
                    match_ ~dots:None env loc pat_tail doc_tail cont
                | Byte a, Byte b when a = b ->
                    match_ ~dots:None env loc pat_tail doc_tail cont
                | _ ->
                    Fail
              in
              match match_result with
              | Complete _ -> match_result
              | Fail ->
                  (* Pattern doesn't match document.
                     Skip document's head node if we're allowed to. *)
                  if loc_matches_dots ~dots loc then
                    match_ ~dots env last_loc pat doc_tail cont
                  else
                    Fail

let starts_after last_loc loc =
  let _, last_pos = last_loc in
  let pos, _ = loc in
  Loc.Pos.compare last_pos pos < 0

let rec get_start_loc (doc : Doc_AST.node list) =
  match doc with
  | [] -> None
  | Atom (loc, _) :: _ -> Some loc
  | List doc1 :: doc2 ->
      match get_start_loc doc1 with
      | None -> get_start_loc doc2
      | res -> res

let rec fold acc (doc : Doc_AST.node list) f =
  match doc with
  | [] -> acc
  | Atom (loc, _) :: doc_tail ->
      let acc = f acc loc doc in
      fold acc doc_tail f
  | List doc1 :: doc2 ->
      let acc = fold acc doc1 f in
      fold acc doc2 f

(*
   Search for non-overlapping matches.
   last_loc is a forbidden start location. Any attempt to match must start
   from a location after last_loc.
*)
let search pat doc =
  fold [] doc (fun matches start_loc doc ->
    let ok_loc =
      match matches with
      | [] -> true
      | (_, last_loc) :: _ -> starts_after last_loc start_loc
    in
    if ok_loc then
      match
        match_ ~dots:None Env.empty start_loc pat doc full_match
      with
      | Complete (_env, last_loc) -> (start_loc, last_loc) :: matches
      | Fail -> matches
    else
      matches
  )
  |> List.rev

let ansi_highlight s =
  match s with
  | "" -> s
  | s -> ANSITerminal.(sprintf [Bold; green] "%s" s)

let print ?(highlight = false) src matches =
  let line_prefix =
    match Src_file.source src with
    | File path -> sprintf "%s:" path
    | Stdin | String | Channel -> ""
  in
  let highlight_fun =
    if highlight then Some ansi_highlight
    else None
  in
  List.iter (fun (start_loc, end_loc) ->
    if !debug then
      printf "match from %s to %s\n" (Loc.show start_loc) (Loc.show end_loc);
    Src_file.lines_of_loc_range
      ?highlight:highlight_fun
      ~line_prefix
      src start_loc end_loc
    |> print_string;
    print_char '\n'
  ) matches
