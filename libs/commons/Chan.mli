(* Channel on local files with filename attached for easier error reporting *)

(* alt: we could make those types abstract *)

type i = { ic : in_channel; p : Fpath.t }
type o = { oc : out_channel; p : Fpath.t }
