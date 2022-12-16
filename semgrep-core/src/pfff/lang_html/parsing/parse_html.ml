(*  Patrick Doane and Gerd Stolpmann
 *
 * Copyright (C) 2001-2006 Patrick Doane and Gerd Stolpmann
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
open Common2
open Common

open Ast_html

module Flag = Flag_parsing
module Ast = Ast_html
module TH   = Token_helpers_html
module T = Parser_html
module PI = Parse_info
module Dtd = Dtd_simple

(* While porting the original html parser to return an AST with line
 * information, the parser was getting buggy because some of the code
 * was using = or <> which was not working property anymore (because
 * the string of the tags were the same but their position was different)
 * The 2 functions definition below make sure we never use those
 * evil too-generic equality operators
*)
(*
let (=) () () = false
let (<>) () () = false
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * src: most of the code in this file comes from ocamlnet/netstring/.
 * The original CVS ID is:
 * $Id: nethtml.ml 1296 2009-11-18 13:27:41Z ChriS $
 * I've extended it mainly to add position information. I've also
 * moved stuff in dtd.ml and removed the encode/decode and xmap stuff.
 * I've also simplified the code, factorized things.
 *
 * TODO: need complete_parse_info so have good position information
 * in the tokens.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type program_and_tokens = Ast_html.html_tree * Parser_html.token list

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
(*
 * For many languages I have tokens() and parse() functions, but for
 * HTML because the lexical rules are so tied to the parsing rules,
 * this module does not provide a tokens() function.
 *)

(*****************************************************************************)
(* Ocamlnet parser *)
(*****************************************************************************)

(* a small wrapper over ocamlnet *)
(*
let (parse_simple_tree: Ast_html.html_raw -> Ast_html.html_tree2) =
 fun (Ast.HtmlRaw raw) ->
  let ch = new Netchannels.input_string raw in
  Nethtml.parse
    ~return_declarations:true
    ~return_pis:true
    ~return_comments:true
    ch
*)

(*****************************************************************************)
(* Parsing helpers *)
(*****************************************************************************)
exception End_of_scan

(* p_string: whether string literals in quotation marks are allowed *)
let rec skip_space p_string call_scan =
  let tok =
    if p_string
    then call_scan Lexer_html.scan_element_after_Eq
    else call_scan Lexer_html.scan_element
  in
  match tok with
  | T.Space _ -> skip_space p_string call_scan
  | t -> t

(* skip until ">" (or "/>") *)
let rec skip_element call_scan =
  let tok = call_scan Lexer_html.scan_element in
  match tok with
  | T.Relement _
  | T.Relement_empty _
    ->  ()
  | T.EOF _ -> raise End_of_scan
  | _ -> skip_element call_scan


let parse_atts call_scan =

  let rec parse_atts_lookahead next =
    match next with
    | T.Relement _  -> ( [], false )
    | T.Relement_empty _  -> ( [], true )
    | T.Name (tok1, n) ->
        (match skip_space false call_scan with
         | T.Eq _ ->
             (match skip_space true call_scan with
              | T.Name (tok2, v) ->
                  let toks, is_empty =
                    parse_atts_lookahead (skip_space false call_scan) in
                  ((Attr (String.lowercase_ascii n, tok1), Val (v, tok2)) :: toks,
                   is_empty)
              | T.Literal (tok2, v) ->
                  let toks, is_empty =
                    parse_atts_lookahead (skip_space false call_scan) in
                  ((Attr (String.lowercase_ascii n, tok1), Val (v, tok2))::toks,
                   is_empty)
              | T.EOF _ ->
                  raise End_of_scan
              | T.Relement ii ->
                  if !Flag.exn_when_lexical_error
                  then raise (Parse_info.Parsing_error ii)
                  else
                    (* Illegal *)
                    ( [], false )
              | T.Relement_empty ii ->
                  if !Flag.exn_when_lexical_error
                  then raise (Parse_info.Parsing_error ii)
                  else
                    (* Illegal *)
                    ( [], true )
              | t ->
                  if !Flag.exn_when_lexical_error
                  then raise (Parse_info.Parsing_error (TH.info_of_tok t))
                  else
                    (* Illegal *)
                    parse_atts_lookahead (skip_space false call_scan)
             )
         | T.EOF _ ->
             raise End_of_scan
         | T.Relement _ ->
             (* <tag name> <==> <tag name="name"> *)
             ([Attr (String.lowercase_ascii n, tok1),
               Val (String.lowercase_ascii n, Ast.fakeInfo())], false)
         | T.Relement_empty _ ->
             (* <tag name> <==> <tag name="name"> *)
             ([Attr (String.lowercase_ascii n, tok1),
               Val (String.lowercase_ascii n, Ast.fakeInfo())], true)
         | next' ->
             (* assume <tag name ... > <==> <tag name="name" ...> *)
             let toks, is_empty =
               parse_atts_lookahead next' in
             ((Attr (String.lowercase_ascii n, tok1),
               Val (String.lowercase_ascii n, Ast.fakeInfo())) :: toks,
              is_empty)
        )
    | T.EOF _ ->
        raise End_of_scan
    | t ->
        if !Flag.exn_when_lexical_error
        then raise (Parse_info.Parsing_error (TH.info_of_tok t))
        else
          (* Illegal *)
          parse_atts_lookahead (skip_space false call_scan)
  in
  parse_atts_lookahead (skip_space false call_scan)

(* called for 'Special, not is_empty' tag categories, like
 * <script> and <style>. Parse until </name>.
 *
 * todo: this function is very ugly; could perhaps make scan_special
 *  take the name as a parameter and do this loop until find the name
 *  itself.
*)
let parse_special tag call_scan =
  let (Tag (name, _tok1)) = tag in

  let first_tok = ref None in

  let rec aux () =
    match call_scan Lexer_html.scan_special with
    | T.Lelementend (_tok, n) ->
        if String.lowercase_ascii n =$= name
        then ""
        else "</" ^ n ^ aux ()
    | T.EOF _ -> raise End_of_scan
    | T.CdataSpecial (tok, s) ->
        (if !first_tok =*= None then first_tok := Some tok);
        s ^ aux ()
    | t ->
        if !Flag.exn_when_lexical_error
        then raise (Parse_info.Parsing_error (TH.info_of_tok t))
        else
          (* Illegal *)
          aux ()
  in
  let s = aux () in
  let info =
    match !first_tok with
    | None -> Ast.fakeInfo()
    | Some tok -> PI.rewrap_str s tok
  in
  s, info

(*
 * This is very ugly. The reason for this function and the first_tok
 * hack above was that codemap was originally not displaying the color
 * for text inside <script> or <style>. With this function
 * ./pfff -tokens_html and -dump_html will display tokens agreeing
 * with each other.
 *)
let rec merge_cdataspecial_tokens xs =
  match xs with
  | [] -> []
  | T.CdataSpecial (tok, s1)::T.CdataSpecial (_tok, s2)::rest ->
      let str = s1 ^ s2 in
      let tok = PI.rewrap_str str tok in
      merge_cdataspecial_tokens ((T.CdataSpecial (tok, str))::rest)
  | x::xs ->
      x::merge_cdataspecial_tokens xs

(*****************************************************************************)
(* Misc helpers *)
(*****************************************************************************)

let model_of ~dtd_hash (Tag (element_name, _tok)) =
  try
    (match Hashtbl.find dtd_hash element_name with
     | (eclass, Dtd.Sub_exclusions(_,m)) -> eclass, m
     | m -> m
    )
  with Not_found -> (Dtd.Everywhere, Dtd.Any)

let exclusions_of ~dtd_hash (Tag (element_name, _tok)) =
  try
    (match Hashtbl.find dtd_hash element_name with
     | (_eclass, Dtd.Sub_exclusions(l,_)) -> l
     | _ -> []
    )
  with Not_found -> []

let is_possible_subelement
    ~dtd_hash parent_element parent_excl sub_element =
  let (sub_class, _) = model_of ~dtd_hash sub_element in
  let rec eval m =
    match m with
    | Dtd.Inline2     -> sub_class =*= Dtd.Inline
    | Dtd.Block2      ->
        sub_class =*= Dtd.Block  ||
        sub_class =*= Dtd.Essential_block
    | Dtd.Flow       ->
        sub_class =*= Dtd.Inline ||
        sub_class =*= Dtd.Block  ||
        sub_class =*= Dtd.Essential_block
    | Dtd.Elements l ->
        let (Tag (s, _tok)) = sub_element in
        List.mem s l
    | Dtd.Any        -> true
    | Dtd.Or     (m1,m2) -> eval m1 ||     eval m2
    | Dtd.Except (m1,m2) -> eval m1 && not (eval m2)
    | Dtd.Empty      -> false
    | Dtd.Special    -> false
    | Dtd.Sub_exclusions(_,_) -> assert false
  in
  let (Tag (sub_element_str, _tok)) = sub_element in
  (sub_class =*= Dtd.Everywhere) ||
  (
    (not (StringSet.mem sub_element_str parent_excl)) &&
    let (_, parent_model) = model_of ~dtd_hash parent_element in
    eval parent_model
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

exception Found

type element_state = {
  name: Ast.tag;
  atts: (Ast.attr_name * Ast.attr_value) list;
  subs: Ast.html_tree list;

  excl: StringSet.t;
}

let parse2 file =
  Common.with_open_infile file (fun chan ->
    let buf = Lexing.from_channel chan in
    let table     = Parse_info.full_charpos_to_pos_large file in

    let toks = ref [] in
    let call_scan scannerf =
      let tok = scannerf buf in

      let tok = tok |> TH.visitor_info_of_tok (fun ii ->
        { ii with Parse_info.token=
                    (* could assert pinfo.filename = file ? *)
                    match ii.Parse_info.token with
                    | Parse_info.OriginTok pi ->
                        Parse_info.OriginTok
                          (Parse_info.complete_token_location_large file table pi)
                    | Parse_info.FakeTokStr _
                    | Parse_info.Ab
                    | Parse_info.ExpandedTok _
                      -> raise Impossible
        })
      in
      Common.push tok toks;
      tok
    in

    let dtd = Dtd.html40_dtd in
    let dtd_hash = Common.hash_of_list dtd in

    let current = ref {
      name = Tag ("", Ast.fakeInfo()); atts = [];
      subs = []; excl = StringSet.empty
    }
    in

    let stack = Stack.create() in

    (* If the current element is not a possible parent element for sub_name,
     * search the parent element in the stack.
     * Either the new current element is the parent, or there was no
     * possible parent. In the latter case, the current element is the
     * same element as before.
    *)
    let unwind_stack sub_name =
      let backup = Stack.create() in
      let backup_el = !current in
      try
        while not (is_possible_subelement
                     ~dtd_hash !current.name !current.excl sub_name) do

          (* Maybe we are not allowed to end the current element: *)
          let (current_class, _) = model_of ~dtd_hash !current.name in
          if current_class =*= Dtd.Essential_block
          then raise Stack.Empty;

          (* End the current element and remove it from the stack: *)
          let grant_parent = Stack.pop stack in
          (* Save it; may we need it *)
          Stack.push grant_parent backup;

          (* If gp_name is an essential element, we are not allowed to close
           * it implicitly, even if that violates the DTD.
          *)
          current := { grant_parent with subs =
                                           Ast.Element (!current.name,
                                                        !current.atts,
                                                        List.rev !current.subs) :: grant_parent.subs;
                     }
        done;
      with Stack.Empty ->
        (* It did not work! Push everything back to the stack, and
         * resume the old state.
        *)
        while Stack.length backup > 0 do
          Stack.push (Stack.pop backup) stack
        done;
        current := backup_el;
    in

    let rec parse_next () =
      let t = call_scan Lexer_html.scan_document in
      match t with
      | T.TComment info ->
          current := { !current with subs =
                                       (Element(Tag ("--", info),
                                                [Attr ("contents", Ast.fakeInfo()),
                                                 Val (PI.str_of_info info, Ast.fakeInfo())],[]))
                                       ::!current.subs
                     };
          parse_next()
      | T.TDoctype info ->
          current := { !current with subs =
                                       (Element(Tag ("!", info),
                                                [Attr ("contents", Ast.fakeInfo()),
                                                 Val (PI.str_of_info info, Ast.fakeInfo())],[]))
                                       ::!current.subs;
                     };
          parse_next()
      | T.TPi info ->
          current := { !current with subs =
                                       (Element(Tag ("?", info),
                                                [Attr ("contents", Ast.fakeInfo()),
                                                 Val (PI.str_of_info info, Ast.fakeInfo())],[]))
                                       ::!current.subs;
                     };
          parse_next()

      | T.Lelement (tok, name) ->
          let name = Tag (String.lowercase_ascii name, tok) in
          let (_, model) = model_of ~dtd_hash name in
          (match model with
           | Dtd.Empty ->
               let atts, _ = parse_atts call_scan in
               unwind_stack name;
               current := { !current with subs =
                                            (Element(name, atts, [])) :: !current.subs;
                          };
               parse_next()
           | Dtd.Special ->
               let atts, is_empty = parse_atts call_scan in
               unwind_stack name;
               let data =
                 if is_empty
                 then "", Ast.fakeInfo()
                 else begin
                   let d = parse_special name call_scan in
                   (* Read until ">" *)
                   skip_element call_scan;
                   d
                 end
               in
               current := { !current with subs =
                                            (Ast.Element(name, atts, [Ast.Data data]))
                                            :: !current.subs;
                          };
               parse_next()
           | _ ->
               let atts, is_empty = parse_atts call_scan in
               (* Unwind the stack until we find an element which can be
                * the parent of the new element:
               *)
               unwind_stack name;
               if is_empty then
                 (* Simple case *)
                 current := { !current with
                              subs = (Ast.Element(name, atts, [])) :: !current.subs;
                            }
               else begin
                 (* Push the current element on the stack, and this element
                  * becomes the new current element:
                 *)
                 let new_excl = exclusions_of ~dtd_hash name in
                 Stack.push !current stack;
                 current := {
                   name = name;
                   atts = atts;
                   subs = [];
                   excl = StringSet.union (StringSet.of_list new_excl)
                       !current.excl;
                 };
               end;
               parse_next()
          )
      | T.Cdata (tok, data) ->
          current := { !current with subs =
                                       (Ast.Data (data, tok))::!current.subs;
                     };
          parse_next()
      | T.Lelementend (tok, name) ->
          let name = Tag (String.lowercase_ascii name, tok) in
          (* Read until ">" *)
          skip_element call_scan;
          (* Search the element to close on the stack: *)
          let found =
            (Ast.str_of_tag name =$= Ast.str_of_tag !current.name) ||
            try
              Stack.iter
                (fun { name = old_name; _} ->
                   if Ast.str_of_tag name =$= Ast.str_of_tag old_name
                   then raise Found;
                   match model_of ~dtd_hash old_name with
                   |  Dtd.Essential_block, _ -> raise Not_found;
                       (* Don't close essential blocks implicitly *)
                   | _ -> ()
                )
                stack;
              false
            with
            | Found -> true
            | Not_found -> false
          in
          (* If not found, the end tag is wrong. Simply ignore it. *)
          if not found then
            parse_next()
          else begin
            (* If found: Remove the elements from the stack, and append
             * them to the previous element as sub elements
            *)
            while not (Ast.str_of_tag !current.name =$= Ast.str_of_tag name) do
              let old_el = Stack.pop stack in
              current := { old_el with subs =
                                         (Ast.Element (!current.name, !current.atts,
                                                       List.rev !current.subs)) :: old_el.subs;
                         };
            done;
            (* Remove one more element: the element containing the element
             * currently being closed.
            *)
            let old_el = Stack.pop stack in
            current := { old_el with subs =
                                       (Ast.Element (!current.name, !current.atts,
                                                     List.rev !current.subs)) :: old_el.subs;
                       };
            (* Go on *)
            parse_next()
          end
      | T.EOF _ ->
          raise End_of_scan
      | (  T.Other _| T.Literal _| T.Eq _
        | T.Name _| T.Space _| T.Relement_empty _| T.Relement _
        | T.CdataSpecial _
        )
        ->
          (* pad: ???? *)
          parse_next ()
    in

    let xs =
      try
        parse_next();  (* never returns. Will get a warning X *)
        (* assert false *)
      with End_of_scan ->
        (* Close all remaining elements: *)
        while Stack.length stack > 0 do
          let old_el = Stack.pop stack in
          current := { old_el with subs =
                                     Ast.Element (!current.name, !current.atts,
                                                  List.rev !current.subs) :: old_el.subs;
                     };
        done;
        List.rev !current.subs
    in
    Ast.Element (Tag ("__root__", Ast.fakeInfo()), [], xs),
    (!toks |> List.rev |> merge_cdataspecial_tokens)
  )

let parse a =
  Common.profile_code "Parse_html.parse" (fun () -> parse2 a)



(*****************************************************************************)
(* Other entry points *)
(*****************************************************************************)

(* this function is useful mostly for our unit tests *)
let (html_tree_of_string: string -> Ast_html.html_tree) = fun s ->
  let tmpfile = Common.new_temp_file "pfff_html_tree_of_s" "html" in
  Common.write_file tmpfile s;
  let (ast, _toks) = parse tmpfile in
  Common.erase_this_temp_file tmpfile;
  ast
