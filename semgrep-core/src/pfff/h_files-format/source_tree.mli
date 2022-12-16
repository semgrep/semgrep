
type subsystem = SubSystem of string
type dir = Dir of string

type tree_reorganization = (subsystem * dir list) list



val load_tree_reorganization :
  Common.filename -> tree_reorganization

val change_organization:
  tree_reorganization -> Common.filename (* dir *) -> unit

val subsystem_of_dir :
  dir -> tree_reorganization -> subsystem
