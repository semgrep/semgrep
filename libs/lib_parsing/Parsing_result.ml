(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2020 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Some helpers for the different lexers and parsers in pfff *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* a parser can also "return" an exception like Lexical_error,
 * or Parsing_error (unless Flag_parsing.error_recovery is true).
 *)
type ('ast, 'toks) t = {
  ast : 'ast;
  (* Note that the token list contains usually also the comment-tokens *)
  tokens : 'toks list;
  stat : Parsing_stat.t;
}
