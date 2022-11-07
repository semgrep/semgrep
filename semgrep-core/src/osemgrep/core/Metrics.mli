module State : sig
  (*
     Configures metrics upload.

     ON - Metrics always sent
     OFF - Metrics never sent
     AUTO - Metrics only sent if config is pulled from the server
  *)
  type t = On | Off | Auto

  (* For Cmdliner *)
  val converter : t Cmdliner.Arg.conv
end
