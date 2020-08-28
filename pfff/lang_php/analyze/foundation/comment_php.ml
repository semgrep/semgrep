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

module PI = Parse_info
module TH = Token_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * A few types and helpers related to comment analysis. 
 * Should perhaps at one point parse even more comments and have 
 * tokens such as TWord, TAnnot, etc.
 * 
 * Note that the T_COMMENT and T_DOC_COMMENT tokens do not contain
 * the final newline character. This will be tokenized as a separate
 * TNewline.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* 
 * We sometimes need to analyze comments and modify them, but it can be
 * painful as PHP support different form of PHP comments, and people use
 * different style in it. We want any modification to be harmonious with
 * the rest of the comment, hence the need to better parsing of comments.
 *)
type comment = 
  | DocBlock of 
      string list (* without the leading ' * ' and '/**' and '*/' *) *
      bool (* use '*/'  or '**/' as end mark *)
  | MultiLineSlashStar of string list (* without the leading ' * ' *)
  | SingleLineSlashStar of string (* without the  enclosing '/* ... */' *)
  | SingleLineSlashSlash of string (* without the '// ' *)
  | OtherStyle of string (* raw *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let strip_comment_marks s = 
  match () with
  (* single line comment using /* */ syntax, stupid but it happens *)
  | _ when s =~ "/\\*\\(.*\\)\\*/" -> Common.matched1 s
  | _ when s =~ "^//[ ]*\\(.*\\)" -> Common.matched1 s

  (* must be put after previous cases *)
  | _ when s =~ "^[ *]*\\(.*\\)" -> Common.matched1 s
  | _ -> s

(*
let _ = example (strip_comment_marks "// @emails" = "@emails")
(* when the comment is part of a multiline comment, people use '*' for
 * esthetic reason, actually just like in this comment *)
let _ = example (strip_comment_marks "* @emails"  = "@emails")
let _ = example (strip_comment_marks " @emails foo"  = "@emails foo")
*)

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)
let (parse_comment: string -> comment) = fun s ->
  match () with
  | _ when s =~ "^// \\(.*\\)" ->
      SingleLineSlashSlash (Common.matched1 s)
  | _ when s =~ "^/\\* \\(.*\\) \\*/" ->
      SingleLineSlashStar (Common.matched1 s)

  | _ when s =~ "^/\\*.*" ->
      let xs = Common2.lines s in
      if List.length xs <= 2 then begin
        pr2 ("wrong docblock comment: " ^ s);
        OtherStyle s
      end
      else
        (match Common2.head_middle_tail xs with
        | start_comment, ys, end_comment -> 

            if not (List.mem start_comment ["/**"; "/*"]) || 
               not (List.mem end_comment [" */"; " **/"])
            then begin 
              pr2 ("wrong comment: " ^ s);
              OtherStyle s
            end
            else begin 
              let ys' = ys |> Common.map_filter (fun s ->
                if s =~ " \\*$" 
                then Some ""
                else 
                  if s =~ " \\* \\(.*\\)"
                  then Some (Common.matched1 s)
                else None
              ) in
              if List.length ys <> List.length ys'
              then begin 
                pr2 ("wrong comment: " ^ s);
                OtherStyle s
              end else
                (match start_comment, end_comment with
                | "/**", " */" -> DocBlock (ys', true)
                | "/**", " **/" -> DocBlock (ys', false)
                | "/*", _ -> MultiLineSlashStar ys'
                | _ -> raise Impossible
                )
            end
        )
  | _ -> 
      pr2 ("unknown comment format: " ^ s);
      OtherStyle s

(*
let _ = example(parse_comment "/**\n * foo\n */" = (DocBlock (["foo"],true)))
let _ = example(parse_comment "/*\n * foo\n */" = (MultiLineSlashStar ["foo"]))
*)

(*****************************************************************************)
(* UnParsing *)
(*****************************************************************************)
let gen_space indent = 
  (Common2.repeat " " indent) |> Common.join ""

let (unparse_comment: ?indent:int -> comment -> string) = 
 fun ?(indent=0) m ->
  match m with
  | DocBlock (xs, b) ->
      (["/**"] @
      (xs |> List.map (fun s -> 
        if s = ""
        then " *"
        else 
          spf "%s * %s" (gen_space indent) s)) @
      (if b
      then [spf "%s */" (gen_space indent)]
      else [spf "%s **/" (gen_space indent)]
      )
      ) |> Common2.unlines
  | _ -> 
      raise Todo

(*      
let _ = example(unparse_comment (DocBlock (["foo"],true)) = "/**\n * foo\n */\n" )
let _ = example(unparse_comment (DocBlock ([""],true)) = "/**\n *\n */\n" )
*)
    
(*****************************************************************************)
(* aux *)
(*****************************************************************************)
let (comment_style_new_line: comment -> string) = fun m ->
  match m with
  | DocBlock _ -> " * "
  | SingleLineSlashSlash _ -> "// "
  | MultiLineSlashStar _ -> " * "
  | _ -> raise Todo

(* hmmm could also use an array ... *)
let (index_comment: comment -> (int * string) list) = fun m ->
  match m with
  | SingleLineSlashSlash s -> [0, s]
  | DocBlock (xs,_) | MultiLineSlashStar xs -> 
      Common.index_list_1 xs |> List.map (fun (s, i) -> (i, s))
  | _ -> raise Todo

(*
let _ = example(index_comment (SingleLineSlashSlash "foo") = [0, "foo"])
let _ = example(index_comment (DocBlock (["foo"],true)) = [1, "foo"])
*)

let comments_of_file file =
  let toks = Parse_php.tokens file in
  toks |> Common.map_filter (function
  | Parser_php.T_COMMENT info
  | Parser_php.T_DOC_COMMENT info
      -> Some info
  | _ -> None
  )

(* todo? some code duplication with deadcode_php.ml 
 * todo: optimize things ...
 *)
let comment_before tok all_toks =
  let pos = Parse_info.pos_of_info tok in
  let before = 
    all_toks |> Common2.take_while (fun tok2 ->
      let info = TH.info_of_tok tok2 in
      let pos2 = PI.pos_of_info info in
      pos2 < pos
    )
  in
  let first_non_space =
    List.rev before |> Common2.drop_while (function
    | Parser_php.TNewline _ | Parser_php.TSpaces _ -> true
    | _ -> false
    )
  in
  match first_non_space with
  | (Parser_php.T_COMMENT ii | Parser_php.T_DOC_COMMENT ii)::_xs ->
      Some ii
  | _ -> None


let comment_after tok all_toks =
  let pos = PI.pos_of_info tok in
  let after = 
    all_toks |> Common2.drop_while (fun tok2 ->
      let info = TH.info_of_tok tok2 in
      let pos2 = PI.pos_of_info info in
      pos2 <= pos
    )
  in
  let first_non_space =
    after |> Common2.drop_while (function
    | Parser_php.TNewline _ | Parser_php.TSpaces _ -> true
    | _ -> false
    )
  in
  match first_non_space with
  | (Parser_php.T_COMMENT ii | Parser_php.T_DOC_COMMENT ii)::_xs ->
      Some ii
  | _ -> None

