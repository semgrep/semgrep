(*
   Turn tokenized lines into a tree, based on:
   - indentation,
   - matching braces within the same line.
*)

open Lexer
open AST

let close_acc acc =
  match List.rev acc with
  | [node] -> node
  | nodes -> List nodes

(*
   Parse a line made of ordinary tokens and of opening and closing braces.
   Failure to close a brace correctly causes a retry, in which the
   opening brace is treated as an ordinary token.
*)
let rec parse_line pending_braces acc (tokens : Lexer.token list)
  : (AST.t * Lexer.token list) option =
  match tokens with
  | [] ->
      (match pending_braces with
       | [] -> Some (close_acc acc, [])
       | _ -> None
      )
  | Atom atom :: tokens ->
      parse_line pending_braces (Atom atom :: acc) tokens
  | Open_paren :: tokens ->
      let res =
        parse_line (Close_paren :: pending_braces) [Atom (Punct '(')] tokens
      in
      (match res with
       | None ->
           parse_line pending_braces (Atom (Punct '(') :: acc) tokens
       | Some (node, tokens) ->
           parse_line pending_braces (node :: acc) tokens
      )
  | Open_bracket :: tokens ->
      let res =
        parse_line (Close_bracket :: pending_braces) [Atom (Punct '[')] tokens
      in
      (match res with
       | None ->
           parse_line pending_braces (Atom (Punct '[') :: acc) tokens
       | Some (node, tokens) ->
           parse_line pending_braces (node :: acc) tokens
      )
  | Open_curly :: tokens ->
      let res =
        parse_line (Close_curly :: pending_braces) [Atom (Punct '{')] tokens
      in
      (match res with
       | None ->
           parse_line pending_braces (Atom (Punct '{') :: acc) tokens
       | Some (node, tokens) ->
           parse_line pending_braces (node :: acc) tokens
      )
  | Close_paren :: tokens ->
      (match pending_braces with
       | Close_paren :: pending_braces ->
           Some (close_acc (Atom (Punct ')') :: acc), tokens)
       | [] ->
           parse_line pending_braces (Atom (Punct ')') :: acc) tokens
       | _ ->
           None
      )
  | Close_bracket :: tokens ->
      (match pending_braces with
       | Close_bracket :: pending_braces ->
           Some (close_acc (Atom (Punct ']') :: acc), tokens)
       | [] ->
           parse_line pending_braces (Atom (Punct ']') :: acc) tokens
       | _ ->
           None
      )
  | Close_curly :: tokens ->
      (match pending_braces with
       | Close_curly :: pending_braces ->
           Some (close_acc (Atom (Punct '}') :: acc), tokens)
       | [] ->
           parse_line pending_braces (Atom (Punct '}') :: acc) tokens
       | _ ->
           None
      )

let parse_line tokens =
  match parse_line [] [] tokens with
  | Some (node, []) -> node
  | Some _ -> assert false
  | None -> assert false

(*
   Interpret a sequence of indented lines.
   Same indentation as previously extends the block,
   More indentation starts a sub-block,
   Less indentation closes the current block.
*)
let rec parse_block ind acc lines =
  match lines with
  | [] ->
      (close_acc acc, [])
  | line :: rem_lines ->
      let new_ind = line.indent in
      if new_ind = ind then
        parse_block ind (parse_line line.tokens :: acc) rem_lines
      else if new_ind < ind then
        List (List.rev acc), lines
      else
        let node, lines =
          parse_block new_ind [parse_line line.tokens] rem_lines in
        parse_block ind (node :: acc) lines

let parse_block lines =
  match parse_block 0 [] lines with
  | node, [] -> node
  | _ -> assert false

let of_lexbuf lexbuf =
  let lines = Lexer.lines lexbuf in
  parse_block lines

let of_channel ic =
  let lexbuf = Lexing.from_channel ic in
  of_lexbuf lexbuf

let of_stdin () = of_channel stdin

let of_file file =
  let ic = open_in file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> of_channel ic)
