(* will generate datalog facts for file in dir, run the doop/souffle engine,
 * and output the varpointto info for the file.
 *)
val gen_facts : Common.filename -> Common.dirname -> unit

(* to debug the IL *)
val dump_il : Common.filename -> unit
