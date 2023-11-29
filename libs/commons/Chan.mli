(* Channel on local files with filename attached for easier error reporting *)

(* alt: we could make those types abstract *)

type i = { ic : In_channel.t; p : Fpath.t }
type o = { oc : Out_channel.t; p : Fpath.t }
