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

(* Map from metavariables to their captured value, which is a Word. *)
module Env = Map.Make (String)
type env = string Env.t

type match_result =
  | Complete of env
  | Incomplete of (env * Pattern_AST.node list)
  | Fail

(*
   Match a pattern against a document tree.
*)
let rec match_
    ~dots
    (env : env)
    (pat : Pattern_AST.node list )
    (doc : Doc_AST.node list) : match_result =
  match pat, doc with
  | [], _ ->
      if dots || doc = [] then
        Complete env
      else
        Fail

  | List pat1 :: pat2, List doc1 :: doc2 ->
      (* Indented block coincides with an indented block in the document.
         These blocks must match, independently from the rest. *)
      (match match_ ~dots:false env pat1 doc1 with
       | Complete env -> match_ ~dots:false env pat2 doc2
       | Incomplete _
       | Fail -> Fail
      )
  | List pat1 :: _, Atom _ :: next_doc ->
      (* Indented block in pattern doesn't match in the document. *)
      assert (pat1 <> []);
      if dots then
        match_ ~dots env pat next_doc
      else
        Fail

  | List pat1 :: _, [] ->
      assert (pat1 <> []);
      Incomplete (env, pat)

  | Dots :: next_pat, doc -> match_ ~dots:true env next_pat doc

  | Atom p :: next_pat, doc ->
      match doc with
      | [] -> Incomplete (env, pat)
      | List sub_doc :: next_doc ->
          (* Indented block in the document doesn't have to match indented
             block in the pattern. We just continue matching in the block. *)
          (match match_ ~dots env pat sub_doc with
           | Complete env -> Complete env
           | Fail -> Fail
           | Incomplete (env, pat) ->
               (* The sub-block was matched but some of the pattern wasn't
                  consumed. We continue, in the sub-block's parent. *)
               match_ ~dots env pat next_doc
          )
      | Atom d :: next_doc ->
          match p, d with
          | Metavar name, Word value ->
              (match Env.find_opt name env with
               | None ->
                   (* First encounter of the metavariable, store its value. *)
                   let env = Env.add name value env in
                   match_ ~dots:false env next_pat next_doc
               | Some value0 ->
                   (* Check if value matches previously captured value. *)
                   if value = value0 then
                     match_ ~dots:false env next_pat next_doc
                   else (* not a match *) if dots then
                     match_ ~dots env pat next_doc
                   else
                     Fail
              )
          | Word a, Word b when a = b ->
              match_ ~dots:false env next_pat next_doc
          | Punct a, Punct b when a = b ->
              match_ ~dots:false env next_pat next_doc
          | Byte a, Byte b when a = b ->
              match_ ~dots:false env next_pat next_doc
          | _ ->
              (* Pattern node doesn't match document node. *)
              if dots then
                (* Skip document node if we're allowed to. *)
                match_ ~dots env pat next_doc
              else
                Fail

let search pat doc =
  match match_ ~dots:true Env.empty (pat @ [Dots]) doc with
  | Complete _env -> true
  | Incomplete _
  | Fail -> false
