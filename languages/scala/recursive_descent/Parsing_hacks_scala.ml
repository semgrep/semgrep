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
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let insert_indentation_tokens toks =
  List.fold_left
    (fun (line, col, acc) tok ->
      let info = TH.info_of_tok tok in
      let line2 = PI.line_of_info info in
      let col2 = PI.col_of_info info in
      match tok with
      (* We ignore any newlines and spaces, because we're trying to estimate
          the relationship between the "real" tokens in the stream.
      *)
      | Nl _
      | Space _ ->
          (line, col, tok :: acc)
      | _ ->
          (* For tokens on the same line, there has been no indentation. *)
          if line2 =*= line then (line2, col, tok :: acc)
            (* This must mean we're on a new line. *)
          else if col2 < col then (line2, col2, tok :: DEDENT :: acc)
          else if col2 > col then (line2, col2, tok :: INDENT :: acc)
          else (line2, col2, tok :: acc))
    (0, 0, []) toks
  |> fun (_, _, acc) -> List.rev acc
