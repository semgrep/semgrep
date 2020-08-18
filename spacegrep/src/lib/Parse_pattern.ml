(*
   Turn tokenized lines into a tree, based on:
   - indentation,
   - matching braces within the same line.
*)

open Lexer
open Pattern_AST

let close_acc acc =
  List.rev acc

type pending_brace =
  | Paren
  | Bracket
  | Curly

(*
   Parse a line made of ordinary tokens and of opening and closing braces.
   Failure to close a brace correctly causes a retry, in which the
   opening brace is treated as an ordinary token.
*)
let rec parse_line pending_braces acc (tokens : Lexer.token list)
  : (Pattern_AST.node list
     * pending_brace list
     * Loc.t (* location of the closing brace, if any *)
     * Lexer.token list) option =
  match tokens with
  | [] ->
      (match pending_braces with
       | [] -> Some (close_acc acc, [], Loc.dummy, [])
       | _ -> None
      )

  | Dots loc :: tokens ->
      parse_line pending_braces (Dots loc :: acc) tokens

  | Atom (loc, atom) :: tokens ->
      parse_line pending_braces (Atom (loc, atom) :: acc) tokens

  | Open_paren open_loc :: tokens ->
      let res = parse_line (Paren :: pending_braces) [] tokens in
      (match res with
       | None ->
           parse_line pending_braces (Atom (open_loc, Punct '(') :: acc) tokens
       | Some (nodes, pending_braces, close_loc, tokens) ->
           parse_line pending_braces
             (Atom (close_loc, Punct ')')
              :: List nodes
              :: Atom (open_loc, Punct '(') :: acc)
             tokens
      )
  | Open_bracket open_loc :: tokens ->
      let res = parse_line (Bracket :: pending_braces) [] tokens in
      (match res with
       | None ->
           parse_line pending_braces (Atom (open_loc, Punct '[') :: acc) tokens
       | Some (nodes, pending_braces, close_loc, tokens) ->
           parse_line pending_braces
             (Atom (close_loc, Punct ']')
              :: List nodes
              :: Atom (open_loc, Punct '[') :: acc)
             tokens
      )
  | Open_curly open_loc :: tokens ->
      let res = parse_line (Curly :: pending_braces) [] tokens in
      (match res with
       | None ->
           parse_line pending_braces (Atom (open_loc, Punct '{') :: acc) tokens
       | Some (nodes, pending_braces, close_loc, tokens) ->
           parse_line pending_braces
             (Atom (close_loc, Punct '}')
              :: List nodes
              :: Atom (open_loc, Punct '{') :: acc) tokens
      )
  | Close_paren close_loc :: tokens ->
      (match pending_braces with
       | Paren :: pending_braces ->
           Some (close_acc acc, pending_braces, close_loc, tokens)
       | (Bracket | Curly) :: _ ->
           None
       | [] ->
           parse_line pending_braces
             (Atom (close_loc, Punct ')') :: acc) tokens
      )
  | Close_bracket close_loc :: tokens ->
      (match pending_braces with
       | Bracket :: pending_braces ->
           Some (close_acc acc, pending_braces, close_loc, tokens)
       | (Paren | Curly) :: _ ->
           None
       | [] ->
           parse_line pending_braces
             (Atom (close_loc, Punct ']') :: acc) tokens
      )
  | Close_curly close_loc :: tokens ->
      (match pending_braces with
       | Curly :: pending_braces ->
           Some (close_acc acc, pending_braces, close_loc, tokens)
       | (Paren | Bracket) :: pending_braces ->
           None
       | [] ->
           parse_line pending_braces
             (Atom (close_loc, Punct '}') :: acc) tokens
      )

(* Try to match braces within the line. This is intended for documents, not
   for patterns. *)
let parse_doc_line tokens : Pattern_AST.node list =
  match parse_line [] [] tokens with
  | Some (nodes, [], _loc, []) -> nodes
  | Some _ -> assert false
  | None -> assert false

(* Interpret braces as regular punctuation. This is intended for patterns. *)
let parse_pattern_line (tokens : Lexer.token list) : Pattern_AST.node list =
  List.map (fun (token : Lexer.token) ->
    match token with
    | Atom (loc, atom) -> Atom (loc, atom)
    | Dots loc -> Dots loc
    | Open_paren loc -> Atom (loc, Punct '(')
    | Close_paren loc -> Atom (loc, Punct ')')
    | Open_bracket loc -> Atom (loc, Punct '(')
    | Close_bracket loc -> Atom (loc, Punct ')')
    | Open_curly loc -> Atom (loc, Punct '{')
    | Close_curly loc -> Atom (loc, Punct '}')
  ) tokens

(*
   Interpret a sequence of indented lines.
   Same indentation as previously extends the block,
   More indentation starts a sub-block,
   Less indentation closes the current block.
*)
let parse_root ~is_doc lines =
  let parse_line =
    if is_doc then parse_doc_line
    else parse_pattern_line
  in
  let rec parse_block
      ind (acc : Pattern_AST.node list) (lines : Lexer.line list)
    : Pattern_AST.node list * Lexer.line list =

    match lines with
    | [] ->
        (close_acc acc, [])
    | line :: rem_lines ->
        let new_ind = line.indent in
        if new_ind = ind then
          parse_block ind
            (List.rev_append (parse_line line.tokens) acc)
            rem_lines
        else if new_ind < ind then
          close_acc acc, lines
        else
          let nodes, lines =
            parse_block new_ind (List.rev (parse_line line.tokens)) rem_lines
          in
          parse_block ind (List nodes :: acc) lines
  in
  match parse_block 0 [] lines with
  | nodes, [] -> nodes
  | _ -> assert false

let of_lexbuf ?(is_doc = false) lexbuf =
  let lines = Lexer.lines lexbuf in
  parse_root ~is_doc lines

let of_string ?(is_doc = false) s =
  let lexbuf = Lexing.from_string s in
  of_lexbuf ~is_doc lexbuf

let of_channel ?(is_doc = false) ic =
  let lexbuf = Lexing.from_channel ic in
  of_lexbuf ~is_doc lexbuf

let of_stdin ?(is_doc = false) () = of_channel ~is_doc stdin

let of_file ?(is_doc = false) file =
  let ic = open_in file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> of_channel ~is_doc ic)
