(* Run jobs in parallel, using number of cores specified with -j.
 *
 * Note that the jobs are currently run in forked process, so while
 * the job will inherit the memory from the parent process, and modification
 * in the child process of the memory (e.g., modifying a global hash table)
 * will not be seen by the parent! So the job function should be a pure
 * function!
 *)
val map_targets__run_in_forked_process_do_not_modify_globals :
  int (* ncores *) ->
  (Target.t -> 'a) ->
  (* job function *) Target.t list ->
  'a list

val map_regular_targets__run_in_forked_process_do_not_modify_globals :
  int (* ncores *) ->
  (Target.regular -> 'a) ->
  (* job function *) Target.regular list ->
  'a list
