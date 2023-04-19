(* Brandon Wu
 *
 * Copyright (C) 2023 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 *)
open Common
module Flag = Flag_parsing
module T = Token_scala
module TH = Token_helpers_scala
module PI = Parse_info
module PS = Parsing_stat

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to insert INDENT and DEDENT tokens to the token
 * stream, so that we can detect whitespace-sensitive indentation-delimited
 * blocks.
 *
 * In reality, the official Scala parser actually does this at parse time,
 * the same way that we "dynamically" insert newlines as we parse the token
 * stream.
 *
 * Our approach is that we will insert these tokens ahead of parsing time,
 * modifying the token stream wholesale.
 *
 * My (brandon) personal belief is that this is equivalent, and also easier to
 * understand, because we can know ahead of time what indent and dedent tokens
 * are present in thes tream, should we run into errors in the future.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  line : int;
  indent_info : (int * int) list; (* (col, idx) *)
  acc : T.t list;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* From the Scala 3 compiler:
   https://github.com/lampepfl/dotty/blob/865aa639c98e0a8771366b3ebc9580cc8b61bfeb/compiler/src/dotty/tools/dotc/parsing/Scanners.scala#L516

   The following tokens can start an indentation region:
   :  =  =>  <-  if  then  else  while  do  try  catch  finally  for  yield  match
*)

let can_start_indentation_tok tok =
  match tok with
  | T.COLON _
  | EQUALS _
  | ARROW _
  | LARROW _
  | Kif _
  | Kwith _
  (* there is no "then" keyword, because it's only a keyword in Scala 3 *)
  (* for now, let's just say "then" doesn't trigger an indentation region *)
  | Kelse _
  | Kwhile _
  | Kdo _
  | Ktry _
  | Kcatch _
  | Kfinally _
  | Kfor _
  | Kyield _
  | Kmatch _ ->
      true
  | _ -> false

(* When comparing whether an indent is far right enough,
   it's not sufficient to compare it to just the last line.

   See examples like

   class Foo
       extends Bar:
     def foo() = 3

   In this case, there should be an INDENT before `def`.
   We will just compare it to previous indentation regions
   instead.
*)
let can_start_indentation cur_col env =
  let rec loop acc =
    match acc with
    | [] -> false
    | T.Nl _ :: rest
    | Space _ :: rest
    | Comment _ :: rest ->
        loop rest
    | t :: _ -> (
        match env.indent_info with
        | [] -> can_start_indentation_tok t
        | (last_indent_col, _) :: _ ->
            cur_col > last_indent_col && can_start_indentation_tok t)
  in
  loop env.acc

(* We want to check whether this current column is to the left of some
   introduced indentation regions.

   Notably, this can end multiple indentation regions, for instance
   in the example of code like:

   class Foo:
     def foo() =
       3
   val x = 2

   where the `val` dedent actually ends two indentation regions.

   To simplify the parsing code, we will emit multiple such DEDENT tokens,
   which will all be consumed at the end of each indentation region.
*)
let produce_dedents env cur_col =
  let rec loop info =
    match info with
    | [] -> None
    | (last_indent_col, last_indent_idx) :: rest ->
        if cur_col < last_indent_col then
          match loop rest with
          | None -> Some ([ last_indent_idx ], rest)
          | Some (indent_idxs, rest) ->
              Some (last_indent_idx :: indent_idxs, rest)
        else None
  in
  (* The infos we see are in order of recency, so for instance we see
      class Foo:
        def foo() =
          3
      then our indent_info will be
      INDENT<def>, INDENT<class>

      then reversing the output dedents here will keep them in the same order, so still
      INDENT<def>, then INDENT<class>

      This is because when we put them into the accumulator, we want to have
      them be in that same order, because `class` is the outermost dedent

      We want VAL, INDENT<def>, INDENT<class> for the accumulator.
  *)
  let* reversed_indents, rest = loop env.indent_info in
  Some (List.rev reversed_indents, rest)

let empty_env = { line = 0; indent_info = []; acc = [] }

(*****************************************************************************)
(* Parsing hacks *)
(*****************************************************************************)

let insert_indentation_tokens toks =
  (* for unique IDs for indents *)
  let count = ref 0 in
  List.fold_left
    (fun env tok ->
      let info = TH.info_of_tok tok in
      let line2 = PI.line_of_info info in
      let col2 = PI.col_of_info info in
      match tok with
      (* We ignore any newlines and spaces, because we're trying to estimate
          the relationship between the "real" tokens in the stream.

         coupling: [can_start_indentation] above
      *)
      | Nl _
      | Space _
      | Comment _ ->
          { env with acc = tok :: env.acc }
      | _ ->
          (* For tokens on the same line, there can be no indentation. *)
          if line2 =*= env.line then
            { env with line = line2; acc = tok :: env.acc }
          else
            (* This must mean we're on a new line. *)
            let indent_info, acc =
              match produce_dedents env col2 with
              | Some (ended_indents, indent_info) ->
                  let introduced_dedents =
                    Common.map (fun idx -> T.DEDENT idx) ended_indents
                  in
                  (indent_info, (tok :: introduced_dedents) @ env.acc)
              | None ->
                  (* If the last token we saw was something that could start an
                     indentation region, and the next token is indented w.r.t. the
                     previous indentation region, then we insert an INDENT.
                     This covers things like:

                     if
                       true

                     but not

                     if
                     true

                     This indent has a unique ID associated with it, which we will use to
                     pair it with its corresponding DEDENT. We also save the current column,
                     which is the width of the indentation region, because we will need to
                     wait until we go to the left of it, to trigger the DEDENT.
                  *)
                  if can_start_indentation col2 env then (
                    let new_idx = !count in
                    incr count;
                    ( (col2, new_idx) :: env.indent_info,
                      tok :: INDENT new_idx :: env.acc ))
                  else (env.indent_info, tok :: env.acc)
            in
            { line = line2; indent_info; acc })
    empty_env toks
  |> fun env -> List.rev env.acc
