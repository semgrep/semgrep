(*
   Pattern parser.

   Handles brace matching and that's pretty much it.
*)

module A = Pat_AST

let parse (tokens : Pat_lexer.token list) : Pat_AST.node list =
  let rec parse_seq_until (acc : Pat_AST.node list) expected_close xs =
    (* Try to close the open brace, if any.

       Ok: When a suitable closing brace is found, the sequence accumulated
       before it is returned, as well as the remaining of the input.
       Error: When no suitable closing brace is found. The accumulated
       sequence is returned.
    *)
    match (xs : Pat_lexer.token list) with
    | [] -> (
        match expected_close with
        | None -> Ok (List.rev acc, [])
        | Some _ -> Error ())
    | ELLIPSIS :: xs -> parse_seq_until (Ellipsis :: acc) expected_close xs
    | LONG_ELLIPSIS :: xs ->
        parse_seq_until (Long_ellipsis :: acc) expected_close xs
    | METAVAR id :: xs -> parse_seq_until (Metavar id :: acc) expected_close xs
    | METAVAR_ELLIPSIS id :: xs ->
        parse_seq_until (Metavar_ellipsis id :: acc) expected_close xs
    | LONG_METAVAR_ELLIPSIS id :: xs ->
        parse_seq_until (Long_metavar_ellipsis id :: acc) expected_close xs
    | WORD str :: xs -> parse_seq_until (Word str :: acc) expected_close xs
    | OPEN (open_, new_expected_close) :: xs -> (
        match expected_close with
        | Some exp_close when exp_close = open_ ->
            (* case where an OPEN token such as '"' is in fact a closing
               token *)
            Ok (List.rev acc, xs)
        | __ -> (
            match parse_seq_until [] (Some new_expected_close) xs with
            | Ok (seq, xs) ->
                let acc = A.Bracket (open_, seq, new_expected_close) :: acc in
                parse_seq_until acc expected_close xs
            | Error () ->
                (* unclosed brace, treat it as normal text *)
                let acc = A.Other (String.make 1 open_) :: acc in
                parse_seq_until acc expected_close xs))
    | CLOSE close :: xs -> (
        match expected_close with
        | Some char when char = close ->
            (* closing of a brace like ')' that's not also an opening brace *)
            Ok (List.rev acc, xs)
        | __ ->
            (* closing brace doesn't match the opening brace, treat it as
               normal text *)
            let acc = A.Other (String.make 1 close) :: acc in
            parse_seq_until acc expected_close xs)
    | NEWLINE :: xs -> parse_seq_until (Newline :: acc) expected_close xs
    | OTHER str :: xs -> parse_seq_until (Other str :: acc) expected_close xs
  in
  let ast =
    match parse_seq_until [] None tokens with
    | Ok (seq, []) -> seq
    | Ok _ -> assert false
    | Error () -> assert false
  in
  Pat_AST.check ast;
  ast

let from_string ?source_name conf pat_str =
  let compiled_conf = Pat_lexer.compile conf in
  Pat_lexer.read_string ?source_name compiled_conf pat_str |> parse
