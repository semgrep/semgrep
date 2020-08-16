(*
   Match a parsed pattern against a parsed document.

   Expectations:

   1. A flat (non-indented) pattern may match both a flat and an indented
      document.
   2. An indented pattern may only match an indented document.

   i.e. if the user bothers indenting their pattern, we honor this constraint.

   Example 1
   ---------

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

   Example 2
   ---------

   pattern:

     f(x) {
       a;
     }

   matching document:

     f(x) {
         a;
     }

   non-matching document:

     f(x) { a; }

   because the latter non-matching document is interpreted as:

     f(a)
       {
       a;
       }
*)

open Doc_AST
open Pattern_AST

type match_result =
  | Complete
  | Incomplete of Pattern_AST.node list
  | Fail

(*
   Match a pattern against a document tree.
*)
let rec match_
    ~dots
    (pat : Pattern_AST.node list )
    (doc : Doc_AST.node list) : match_result =
  match pat, doc with
  | [], _ -> Complete

  | List pat1 :: pat2, List doc1 :: doc2 ->
      (* Indented block coincides with an indented block in the document.
         These blocks must match, independently from the rest. *)
      (match match_ ~dots:false pat1 doc1 with
       | Complete -> match_ ~dots:false pat2 doc2
       | Incomplete _
       | Fail -> Fail
      )
  | List pat1 :: _, Atom _ :: next_doc ->
      (* Indented block in pattern doesn't match in the document. *)
      assert (pat1 <> []);
      if dots then
        match_ ~dots pat next_doc
      else
        Fail

  | List pat1 :: _, [] ->
      assert (pat1 <> []);
      Incomplete pat

  | Atom p :: next_pat, doc ->
      match doc with
      | [] -> Incomplete pat
      | List sub_doc :: next_doc ->
          (* Indented block in the document doesn't have to match indented
             block in the pattern. We just continue matching in the block. *)
          (match match_ ~dots pat sub_doc with
           | Complete -> Complete
           | Fail -> Fail
           | Incomplete pat ->
               (* The sub-block was matched but some of the pattern wasn't
                  consumed. We continue, in the sub-block's parent. *)
               match_ ~dots pat next_doc
          )
      | Atom d :: next_doc ->
          match p, d with
          | Dots, _ -> match_ ~dots:true next_pat doc
          | Metavar _, _ -> failwith "support for '$VAR' was not implemented"
          | Word a, Word b when a = b -> match_ ~dots:false next_pat next_doc
          | Punct a, Punct b when a = b -> match_ ~dots:false next_pat next_doc
          | Byte a, Byte b when a = b -> match_ ~dots:false next_pat next_doc
          | _ ->
              (* Pattern node doesn't match document node. *)
              if dots then
                (* Skip document node if we're allowed to. *)
                match_ ~dots pat next_doc
              else
                Fail

let search pat doc =
  match match_ ~dots:true pat doc with
  | Complete -> true
  | Incomplete _
  | Fail -> false
