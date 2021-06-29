(*
   Turn tokenized lines into a tree, based on:
   - indentation,
   - matching braces within the same line.
*)

open Lexer
open Pattern_AST

type error = { loc : Loc.t; msg : string }

let close_acc acc = List.rev acc

type pending_brace = Paren | Bracket | Curly

let open_paren = Punct '('

let close_paren = Punct ')'

let open_bracket = Punct '['

let close_bracket = Punct ']'

let open_curly = Punct '{'

let close_curly = Punct '}'

let append_block_to_accumulator open_punct close_punct open_loc close_loc
    block_nodes acc =
  match block_nodes with
  | [] -> Atom (close_loc, close_punct) :: Atom (open_loc, open_punct) :: acc
  | block_nodes ->
      Atom (close_loc, close_punct)
      :: List block_nodes
      :: Atom (open_loc, open_punct)
      :: acc

(*
   Parse a line made of ordinary tokens and of opening and closing braces.
   Failure to close a brace correctly causes a retry, in which the
   opening brace is treated as an ordinary token.
*)
let rec parse_line pending_braces acc (tokens : Lexer.token list) :
    ( Pattern_AST.node list
    * pending_brace list
    * Loc.t (* location of the closing brace, if any *)
    * Lexer.token list )
    option =
  match tokens with
  | [] -> (
      match pending_braces with
      | [] -> Some (close_acc acc, [], Loc.dummy, [])
      | _ -> None )
  | Dots (loc, opt_mvar) :: tokens ->
      parse_line pending_braces (Dots (loc, opt_mvar) :: acc) tokens
  | Atom (loc, atom) :: tokens ->
      parse_line pending_braces (Atom (loc, atom) :: acc) tokens
  | Open_paren open_loc :: tokens -> (
      let res = parse_line (Paren :: pending_braces) [] tokens in
      match res with
      | None ->
          parse_line pending_braces (Atom (open_loc, open_paren) :: acc) tokens
      | Some (nodes, pending_braces, close_loc, tokens) ->
          parse_line pending_braces
            (append_block_to_accumulator open_paren close_paren open_loc
               close_loc nodes acc)
            tokens )
  | Open_bracket open_loc :: tokens -> (
      let res = parse_line (Bracket :: pending_braces) [] tokens in
      match res with
      | None ->
          parse_line pending_braces
            (Atom (open_loc, open_bracket) :: acc)
            tokens
      | Some (nodes, pending_braces, close_loc, tokens) ->
          parse_line pending_braces
            (append_block_to_accumulator open_bracket close_bracket open_loc
               close_loc nodes acc)
            tokens )
  | Open_curly open_loc :: tokens -> (
      let res = parse_line (Curly :: pending_braces) [] tokens in
      match res with
      | None ->
          parse_line pending_braces (Atom (open_loc, open_curly) :: acc) tokens
      | Some (nodes, pending_braces, close_loc, tokens) ->
          parse_line pending_braces
            (append_block_to_accumulator open_curly close_curly open_loc
               close_loc nodes acc)
            tokens )
  | Close_paren close_loc :: tokens -> (
      match pending_braces with
      | Paren :: pending_braces ->
          Some (close_acc acc, pending_braces, close_loc, tokens)
      | (Bracket | Curly) :: _ -> None
      | [] ->
          parse_line pending_braces
            (Atom (close_loc, close_paren) :: acc)
            tokens )
  | Close_bracket close_loc :: tokens -> (
      match pending_braces with
      | Bracket :: pending_braces ->
          Some (close_acc acc, pending_braces, close_loc, tokens)
      | (Paren | Curly) :: _ -> None
      | [] ->
          parse_line pending_braces
            (Atom (close_loc, close_bracket) :: acc)
            tokens )
  | Close_curly close_loc :: tokens -> (
      match pending_braces with
      | Curly :: pending_braces ->
          Some (close_acc acc, pending_braces, close_loc, tokens)
      | (Paren | Bracket) :: _pending_braces -> None
      | [] ->
          parse_line pending_braces
            (Atom (close_loc, close_curly) :: acc)
            tokens )

(* Try to match braces within the line. This is intended for documents, not
   for patterns. *)
let parse_doc_line tokens : Pattern_AST.node list =
  match parse_line [] [] tokens with
  | Some (nodes, [], _loc, []) -> nodes
  | Some _ -> assert false
  | None -> assert false

(* Interpret braces as regular punctuation. This is intended for patterns. *)
let parse_pattern_line (tokens : Lexer.token list) : Pattern_AST.node list =
  List.map
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
        match check pat1 with Some err -> Some err | None -> check pat2 )
    | Dots (_, opt_mvar1) :: Dots (loc, Some mvar2) :: _ ->
        let msg =
          Printf.sprintf "Invalid pattern sequence: %s $...%s"
            ( match opt_mvar1 with
            | None -> "..."
            | Some mvar1 -> Printf.sprintf "$...%s" mvar1 )
            mvar2
        in
        Some { loc; msg }
    | _ :: pat -> check pat
  in
  check pat0

let of_lexbuf ?(is_doc = false) lexbuf =
  let lines = Lexer.lines lexbuf in
  let pat = parse_root ~is_doc lines in
  if is_doc then Ok pat
  else match check_pattern pat with None -> Ok pat | Some err -> Error err

let of_src ?is_doc src = Src_file.to_lexbuf src |> of_lexbuf ?is_doc
