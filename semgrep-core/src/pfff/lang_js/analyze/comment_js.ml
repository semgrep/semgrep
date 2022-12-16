(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few types and helpers related to comment analysis.
 *
 * less: at one point we should parse more the comment and have
 * tokens such as TWord, TAnnot, etc. as in CComment.
 *
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Copy paste of Comment_php.strip_comment_marks for now.
 * less: factorize and put stuff in a h_program-lang/comment.ml file
*)
let strip_comment_marks s =
  match () with
  | _ when s =~ "^//[ ]*\\(.*\\)" -> Common.matched1 s
  | _ when s =~ "^[ *]*\\(.*\\)" -> Common.matched1 s
  | _ -> s
