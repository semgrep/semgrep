exception WrongFormat of string
exception UnsupportedFormat of string

(* may raise WrongFormat or UnsupportedFormat *)
val parse :
  Lockfile.kind ->
  Lockfile_xtarget.manifest option ->
  Fpath.t ->
  SCA_dependency.t list

val parse_manifest :
  Manifest.kind -> Fpath.t -> SCA_dependency.manifest_dependency list
