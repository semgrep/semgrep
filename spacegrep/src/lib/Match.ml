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

open Doc_AST
open Pattern_AST

let debug = ref false

(* Map from metavariables to their captured value, which is a Word. *)
module Env = Map.Make (String)
type env = string Env.t

type match_result =
  | Complete of env
  | Fail

(* Continuation that requires the pattern to match the empty document.
   To be used as the last argument of the 'match_' function. *)
let rec full_match ~dots env pat =
  match pat with
  | [] -> Complete env
  | Dots :: pat -> full_match ~dots env pat
  | _ -> Fail

(*
   Match a pattern against a document tree.

   full = indicates we require the pattern to be matched completely when
          reaching the end of the document or sub-document.
   dots = indicates we're allowed to skip the first document node if it doesn't
          match the pattern.
   cont = call to match the rest of the document against the rest of the
          pattern when reaching the end of the current sub-document.
*)
let rec match_
    ~dots
    (env : env)
    (pat : Pattern_AST.node list )
    (doc : Doc_AST.node list)
    (cont : dots:bool -> env -> Pattern_AST.node list -> match_result)
  : match_result =
  if !debug then
    Print_match.print pat doc;
  match pat, doc with
  | [], _ ->
      if dots || doc = [] then
        Complete env
      else
        Fail

  | List pat1 :: pat2, doc ->
      (match doc with
       | [] ->
           (* Nothing left to match against. *)
           assert (pat1 <> []);
           Fail
       | List doc1 :: doc2 ->
           (* Indented block coincides with an indented block in the document.
              These blocks must match, independently from the rest. *)
           (match match_ ~dots:false env pat1 doc1 full_match with
            | Complete env -> match_ ~dots:false env pat2 doc2 cont
            | Fail -> Fail
           )
       | Atom _ :: doc_tail ->
           (* Indented block in pattern doesn't match in the document.
              Skip document node if allowed. *)
           assert (pat1 <> []);
           if dots then
             match_ ~dots env pat doc_tail cont
           else
             Fail
      )

  | Dots :: pat_tail, doc -> match_ ~dots:true env pat_tail doc cont

  | Atom p :: pat_tail, doc ->
      match doc with
      | [] -> cont ~dots env pat
      | doc_head :: doc_tail ->
          match doc_head with
          | List sub_doc ->
              (* Indented block in the document doesn't have to match
                 indented block in the pattern. We just continue matching
                 in the block as if the document was flat. *)
              match_ ~dots env pat sub_doc (fun ~dots env pat ->
                (* The sub-block was matched but some of the pattern wasn't
                   consumed. We continue, in the sub-block's parent. *)
                match_ ~dots env pat doc_tail cont
              )
          | Atom d ->
              let match_result =
                match p, d with
                | Metavar name, Word value ->
                    (match Env.find_opt name env with
                     | None ->
                         (* First encounter of the metavariable,
                            store its value. *)
                         let env = Env.add name value env in
                         match_ ~dots:false env pat_tail doc_tail cont
                     | Some value0 ->
                         (* Check if value matches previously captured
                            value. *)
                         if value = value0 then
                           match_ ~dots:false env pat_tail doc_tail cont
                         else (* not a match *) if dots then
                           match_ ~dots env pat doc_tail cont
                         else
                           Fail
                    )
                | Word a, Word b when a = b ->
                    match_ ~dots:false env pat_tail doc_tail cont
                | Punct a, Punct b when a = b ->
                    match_ ~dots:false env pat_tail doc_tail cont
                | Byte a, Byte b when a = b ->
                    match_ ~dots:false env pat_tail doc_tail cont
                | _ ->
                    Fail
              in
              match match_result with
              | Complete _ -> match_result
              | Fail ->
                  (* Pattern doesn't match document.
                     Skip document's head node if we're allowed to. *)
                  if dots then
                    match_ ~dots env pat doc_tail cont
                  else
                    Fail

let search pat doc =
  match match_ ~dots:true Env.empty (pat @ [Dots]) doc full_match with
  | Complete _env -> true
  | Fail -> false
