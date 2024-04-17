(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Bos.Cmd
 *
 * alt: we could have called it Cmd_, to be consistent with our other
 * extended modules, but Cmd is not used, and there is no ambiguity with
 * Bos.Cmd so Cmd.ml it is.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type name = Name of string [@@deriving show]
type args = string list [@@deriving show]
type t = name * args [@@deriving show]

(* shortcut *)
type run_status = Bos.OS.Cmd.run_status

(*****************************************************************************)
(* API *)
(*****************************************************************************)
(* Note that we use here just Bos.Cmd, which is safe, and not Bos.OS.Cmd
 * which actually execute stuff and is so unsafe (and people should prefer
 * to use CapExec.ml)
 *)

let to_cmd (Name str, args) = Bos.Cmd.(v str %% of_list args)
let bos_apply f x = f (to_cmd x)
let to_string = bos_apply Bos.Cmd.to_string

(* old:
   let empty = Bos.Cmd.empty
   let of_list = Bos.Cmd.of_list
   let pp = Bos.Cmd.pp
   let v = Bos.Cmd.v
   let ( % ) = Bos.Cmd.( % )
   let ( %% ) = Bos.Cmd.( %% )
*)
