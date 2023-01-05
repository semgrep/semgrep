(*
   Configures metrics upload.

   ON - Metrics always sent
   OFF - Metrics never sent
   AUTO - Metrics only sent if config is pulled from the server
*)
type state = On | Off | Auto [@@deriving show]

(* For Cmdliner *)
val converter : state Cmdliner.Arg.conv

type t = {
  mutable is_using_registry : bool;
  mutable payload : unit;
  state : state;
}

val default : t

(* global you should not access directly *)
val g : t ref

(* initialize g that is then modified by the add_xxx functions
 * below and finally accessed in send() further below.
 *)
val configure : state -> unit
