(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

open Ast_html

open Entity_code open Highlight_code

module T = Parser_html

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
*)
let visit_toplevel ~tag_hook _prefs (toplevel, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in
  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *)
  (* -------------------------------------------------------------------- *)

  let rec visit = function

    | Element ((Tag (s_tag, _tok_t)), attrs, xs) ->
        attrs |> List.iter
          (fun (Attr (s_attr, _tok_a), (Val (_s_val, tok_v))) ->
             match s_attr with
             | "href" | "xmlns" -> tag tok_v EmbededUrl
             | "id" -> tag tok_v (Local Def)
             | _ -> ()
          );
        (match s_tag, xs with
         | "pre", _ ->
             xs |> List.iter (function
               | Element _ -> raise Impossible
               | Data (_s, tok) -> tag tok Verbatim
             )
         | "script", _ ->
             xs |> List.iter (function
               | Element _ -> raise Impossible
               | Data (_s, tok) -> tag tok EmbededCode
             )
         | "style", _ ->
             xs |> List.iter (function
               | Element _ -> raise Impossible
               | Data (_s, tok) -> tag tok EmbededStyle
             )

         | "h1", _ ->
             xs |> List.iter (function
               | Element _ -> () | Data (_s, tok) -> tag tok CommentSection1
             )
         | "h2", _ ->
             xs |> List.iter (function
               | Element _ -> () | Data (_s, tok) -> tag tok CommentSection2
             )
         | "h3", _ ->
             xs |> List.iter (function
               | Element _ -> () | Data (_s, tok) -> tag tok CommentSection3
             )
         | _ -> ()
        );
        xs |> List.iter visit
    | Data _ -> ()
  in
  visit toplevel;


  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)
  (*
   * note: all TCommentSpace are filtered in xs so easier to write
   * rules (but regular comments are kept as well as newlines).
   *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  (* -------------------------------------------------------------------- *)
  toks |> List.iter (fun tok ->
    match tok with
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Comment

    | T.Space _ii -> ()
    | T.EOF _ii -> ()

    | T.Eq ii -> tag ii Punctuation

    | T.Lelement (ii, _s) ->
        (* todo: different color depending on element ? *)
        tag ii Keyword

    | T.Lelementend (ii, _s) ->
        (* todo: better category *)
        tag ii (Entity (Module, (Use2 fake_no_use2)));

    | T.Name (ii, _s) ->
        (* todo: different color depending on attr ? *)
        tag ii TypeInt

    | T.Relement ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Keyword
    | T.Relement_empty _ii ->
        ()

    | T.Literal (ii, _s) ->
        (* can be a href *)
        if not (Hashtbl.mem already_tagged ii)
        then tag ii String

    | T.Other ii -> tag ii NotParsed

    | T.Cdata (_ii, _s) | T.CdataSpecial (_ii, _s) ->
        (* can be js code, css code *)
(*
        if not (Hashtbl.mem already_tagged ii)
        then () (* tag ii String ? *)
*)
        ()

    | T.TPi _ii
    | T.TDoctype _ii
      ->
        ()
  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)
  (* -------------------------------------------------------------------- *)

  ()
