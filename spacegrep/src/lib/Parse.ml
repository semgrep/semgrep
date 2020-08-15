(*
   Turn tokenized lines into a tree, based on:
   - indentation,
   - matching braces within the same line.
*)

open Lexer
open AST

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
  : (AST.node list * pending_brace list * Lexer.token list) option =
  match tokens with
  | [] ->
      (match pending_braces with
       | [] -> Some (close_acc acc, [], [])
       | _ -> None
      )
  | Atom atom :: tokens ->
      parse_line pending_braces (Atom atom :: acc) tokens
  | Open_paren :: tokens ->
      let res =
        parse_line (Paren :: pending_braces) [Atom (Punct '(')] tokens
      in
      (match res with
       | None ->
           parse_line pending_braces (Atom (Punct '(') :: acc) tokens
       | Some (nodes, pending_braces, tokens) ->
           parse_line pending_braces (List nodes :: acc) tokens
      )
  | Open_bracket :: tokens ->
      let res =
        parse_line (Bracket :: pending_braces) [Atom (Punct '[')] tokens
      in
      (match res with
       | None ->
           parse_line pending_braces (Atom (Punct '[') :: acc) tokens
       | Some (nodes, pending_braces, tokens) ->
           parse_line pending_braces (List nodes :: acc) tokens
      )
  | Open_curly :: tokens ->
      let res =
        parse_line (Curly :: pending_braces) [Atom (Punct '{')] tokens
      in
      (match res with
       | None ->
           parse_line pending_braces (Atom (Punct '{') :: acc) tokens
       | Some (nodes, pending_braces, tokens) ->
           parse_line pending_braces (List nodes :: acc) tokens
      )
  | Close_paren :: tokens ->
      (match pending_braces with
       | Paren :: pending_braces ->
           Some (close_acc (Atom (Punct ')') :: acc), pending_braces, tokens)
       | (Bracket | Curly) :: _ ->
           None
       | [] ->
           parse_line pending_braces (Atom (Punct ')') :: acc) tokens
      )
  | Close_bracket :: tokens ->
      (match pending_braces with
       | Bracket :: pending_braces ->
           Some (close_acc (Atom (Punct ']') :: acc), pending_braces, tokens)
       | (Paren | Curly) :: _ ->
           None
       | [] ->
           parse_line pending_braces (Atom (Punct ']') :: acc) tokens
      )
  | Close_curly :: tokens ->
      (match pending_braces with
       | Curly :: pending_braces ->
           Some (close_acc (Atom (Punct '}') :: acc), pending_braces, tokens)
       | (Paren | Bracket) :: pending_braces ->
           None
       | [] ->
           parse_line pending_braces (Atom (Punct '}') :: acc) tokens
      )

let parse_line tokens : AST.node list =
  match parse_line [] [] tokens with
  | Some (nodes, [], []) -> nodes
  | Some _ -> assert false
  | None -> assert false

(*
   Interpret a sequence of indented lines.
   Same indentation as previously extends the block,
   More indentation starts a sub-block,
   Less indentation closes the current block.
*)
let rec parse_block ind (acc : AST.node list) (lines : Lexer.line list)
  : AST.node list * Lexer.line list =
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
          parse_block new_ind (List.rev (parse_line line.tokens)) rem_lines in
        parse_block ind (List nodes :: acc) lines

let parse_root lines =
  match parse_block 0 [] lines with
  | nodes, [] -> nodes
  | _ -> assert false

let of_lexbuf lexbuf =
  let lines = Lexer.lines lexbuf in
  parse_root lines

let of_string s =
  let lexbuf = Lexing.from_string s in
  of_lexbuf lexbuf

let of_channel ic =
  let lexbuf = Lexing.from_channel ic in
  of_lexbuf lexbuf

let of_stdin () = of_channel stdin

let of_file file =
  let ic = open_in file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> of_channel ic)
