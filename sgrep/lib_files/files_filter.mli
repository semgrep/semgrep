
type filters

val mk_filters: 
  excludes: string list -> includes: string list -> exclude_dirs: string list->
  filters

val filter: filters -> Common.filename list -> Common.filename list

