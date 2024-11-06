(*
   Parse a spacegrep pattern or target ("document")

   Turn tokenized lines into a tree, based on:
   - indentation,
   - matching delimiters within the same line.
*)

open Lexer
open Pattern_AST
module Log = Log_spacegrep.Log

type error = { loc : Loc.t; msg : string }

let close_acc acc = List.rev acc

type pending_delimiter = Paren | Bracket | Curly

let open_paren = Punct '('
let close_paren = Punct ')'
let open_bracket = Punct '['
let close_bracket = Punct ']'
let open_curly = Punct '{'
let close_curly = Punct '}'

let append_block_to_accumulator open_punct close_punct open_loc close_loc
    closed_properly block_nodes acc =
  if closed_properly then
    match block_nodes with
    | [] -> Atom (close_loc, close_punct) :: Atom (open_loc, open_punct) :: acc
    | block_nodes ->
        Atom (close_loc, close_punct)
        :: List block_nodes
        :: Atom (open_loc, open_punct)
        :: acc
  else List.rev_append block_nodes (Atom (open_loc, open_punct) :: acc)

(*
   Parse a line made of ordinary tokens and of opening and closing delimiters.

   pending_delim: open delimiter that needs closing
   acc: accumulator of nodes which are either atoms or blocks
   tokens: what's left to read in the line of input

   When running into an opening delimiter, we add an opening delimiter
   to the stack of pending delimiters and scan the input until we
   encounter the closing delimiter. parse_line therefore returns at
   the end of each block.
*)
let rec parse_line pending_delim acc (tokens : Lexer.token list) :
    Pattern_AST.node list
    * Loc.t (* location of the closing delimiter, if any *)
    * Lexer.token list
    * bool (* whether the expected closing delimiter was found *) =
  match tokens with
  | [] ->
      let closed_properly =
        match pending_delim with
        | None -> true
        | Some _ ->
            Log.debug (fun m -> m "warning: unclosed delimiter");
            false
      in
      (close_acc acc, Loc.dummy, [], closed_properly)
  | Dots (loc, opt_mvar) :: tokens ->
      parse_line pending_delim (Dots (loc, opt_mvar) :: acc) tokens
  | Atom (loc, atom) :: tokens ->
      parse_line pending_delim (Atom (loc, atom) :: acc) tokens
  | Open_paren open_loc :: tokens ->
      (* add an element to the stack and parse the input until the end of
         the block marked by the closing delimiter *)
      let nodes, close_loc, tokens, closed_properly =
        parse_line (Some Paren) [] tokens
      in
      (* add the parenthesized block as a 'List' node to the current
         sequence of nodes *)
      let acc =
        append_block_to_accumulator open_paren close_paren open_loc close_loc
          closed_properly nodes acc
      in
      parse_line pending_delim acc tokens
  | Open_bracket open_loc :: tokens ->
      (* same code structure as for Open_paren *)
      let nodes, close_loc, tokens, closed_properly =
        parse_line (Some Bracket) [] tokens
      in
      let acc =
        append_block_to_accumulator open_bracket close_bracket open_loc
          close_loc closed_properly nodes acc
      in
      parse_line pending_delim acc tokens
  | Open_curly open_loc :: tokens ->
      (* same code structure as for Open_paren *)
      let nodes, close_loc, tokens, closed_properly =
        parse_line (Some Curly) [] tokens
      in
      let acc =
        append_block_to_accumulator open_curly close_curly open_loc close_loc
          closed_properly nodes acc
      in
      parse_line pending_delim acc tokens
  | Close_paren close_loc :: tokens -> (
      match pending_delim with
      | Some Paren ->
          (* a closing parenthesis was indeed expected: close the block *)
          (close_acc acc, close_loc, tokens, true)
      | Some (Bracket | Curly)
      | None ->
          (* a closing parenthesis was not expected: treat it as an ordinary
             character and continue *)
          Log.debug (fun m -> m "warning: lone closing parenthesis ')'");
          parse_line pending_delim (Atom (close_loc, close_paren) :: acc) tokens
      )
  | Close_bracket close_loc :: tokens -> (
      (* same code structure as for Close_paren *)
      match pending_delim with
      | Some Bracket -> (close_acc acc, close_loc, tokens, true)
      | Some (Paren | Curly)
      | None ->
          Log.debug (fun m -> m "warning: lone closing bracket ']'");
          parse_line pending_delim
            (Atom (close_loc, close_bracket) :: acc)
            tokens)
  | Close_curly close_loc :: tokens -> (
      (* same code structure as for Close_paren *)
      match pending_delim with
      | Some Curly -> (close_acc acc, close_loc, tokens, true)
      | Some (Paren | Bracket)
      | None ->
          Log.debug (fun m -> m "warning: lone closing brace '}'");
          parse_line pending_delim (Atom (close_loc, close_curly) :: acc) tokens
      )

(* Try to match delimiters within the line. This is intended for documents, not
   for patterns. *)
let parse_doc_line tokens : Pattern_AST.node list =
  match parse_line None [] tokens with
  | nodes, _loc, [], _closed_properly -> nodes
  | _ -> assert false

(* Interpret delimiters as regular punctuation.
   This is intended for patterns. *)
let parse_pattern_line (tokens : Lexer.token list) : Pattern_AST.node list =
  List_.map
    (fun (token : Lexer.token) ->
      match token with
      | Atom (loc, atom) -> Atom (loc, atom)
      | Dots (loc, opt_mvar) -> Dots (loc, opt_mvar)
      | Open_paren loc -> Atom (loc, open_paren)
      | Close_paren loc -> Atom (loc, close_paren)
      | Open_bracket loc -> Atom (loc, open_bracket)
      | Close_bracket loc -> Atom (loc, close_bracket)
      | Open_curly loc -> Atom (loc, open_curly)
      | Close_curly loc -> Atom (loc, close_curly))
    tokens

(*
   Interpret a sequence of indented lines.
   Same indentation as previously extends the block,
   More indentation starts a sub-block,
   Less indentation closes the current block.
*)
let parse_root ~is_doc lines =
  let parse_line = if is_doc then parse_doc_line else parse_pattern_line in
  let rec parse_block ind (acc : Pattern_AST.node list)
      (lines : Lexer.line list) : Pattern_AST.node list * Lexer.line list =
    match lines with
    | [] -> (close_acc acc, [])
    | line :: rem_lines ->
        let new_ind = line.indent in
        if new_ind = ind then
          parse_block ind
            (List.rev_append (parse_line line.tokens) acc)
            rem_lines
        else if new_ind < ind then (close_acc acc, lines)
        else
          let nodes, lines =
            parse_block new_ind (List.rev (parse_line line.tokens)) rem_lines
          in
          parse_block ind (List nodes :: acc) lines
  in
  match parse_block 0 [] lines with
  | nodes, [] ->
      (* 'nodes @ [End]' but without stack overflow: *)
      End :: List.rev nodes |> List.rev
  | _ -> assert false

let check_pattern pat0 =
  let rec check = function
    | [] -> None
    | List pat1 :: pat2 -> (
        match check pat1 with
        | Some err -> Some err
        | None -> check pat2)
    | Dots (_, opt_mvar1) :: Dots (loc, Some mvar2) :: _ ->
        let msg =
          Printf.sprintf "Invalid pattern sequence: %s $...%s"
            (match opt_mvar1 with
            | None -> "..."
            | Some mvar1 -> Printf.sprintf "$...%s" mvar1)
            mvar2
        in
        Some { loc; msg }
    | _ :: pat -> check pat
  in
  check pat0

let of_lexbuf ?(is_doc = false) (lexbuf : Lexing.lexbuf) =
  Log.debug (fun m ->
      m "parse spacegrep pattern or target %S" lexbuf.lex_curr_p.pos_fname);
  let lines = Lexer.lines lexbuf in
  let pat = parse_root ~is_doc lines in
  if is_doc then Ok pat
  else
    match check_pattern pat with
    | None -> Ok pat
    | Some err -> Error err

let of_src ?is_doc src = Src_file.to_lexbuf src |> of_lexbuf ?is_doc
