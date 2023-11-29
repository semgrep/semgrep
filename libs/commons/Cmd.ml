(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Bos.Cmd
 *
 * alt: we could have called it Cmd_, to be consistent with our other
 * extended modules, but Cmd is not used, and there is no ambiguity with
 * Bos.Cmd so Cmd.ml it is
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Bos.Cmd.t

(*****************************************************************************)
(* API *)
(*****************************************************************************)
(* Note that we put here just Bos.Cmd, which is safe, and not Bos.OS.Cmd
 * which actually execute stuff and is so unsafe (and people should prefer
 * to use CapExec.ml)
 *)

let empty = Bos.Cmd.empty
let of_list = Bos.Cmd.of_list
let to_string = Bos.Cmd.to_string
let pp = Bos.Cmd.pp
let v = Bos.Cmd.v
let ( % ) = Bos.Cmd.( % )
let ( %% ) = Bos.Cmd.( %% )

(* FORBIDDEN:
 *  - ??
 *)
