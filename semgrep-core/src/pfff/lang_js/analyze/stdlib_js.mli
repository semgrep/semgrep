(* stdlib.js *)
val path_stdlib : Common.filename

(* generate stdlib.js *)
val extract_from_sources : Common.filename list -> Common.dirname -> unit
