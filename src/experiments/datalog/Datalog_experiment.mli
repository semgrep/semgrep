(* will generate datalog facts for file in dir, run the doop/souffle engine,
 * and output the varpointto info for the file.
 *)
val gen_facts : Fpath.t (* filename *) -> Fpath.t (* a directory *) -> unit
