(* Run jobs in parallel, using number of cores specified with -j.
 *
 * Note that the jobs are currently run in forked process, so while
 * the job will inherit the memory from the parent process, any modification
 * in the child process of the memory (e.g., modifying a global hash table)
 * will not be seen by the parent! So the job function should be a pure
 * function!
 * See: Parmap_.mli for why this returns a result list!
 *)
val map_targets__run_in_forked_process_do_not_modify_globals :
  < Cap.fork > ->
  int (* ncores *) ->
  (Target.t -> 'a) ->
  (* job function *) Target.t list ->
  ('a, Target.t * Core_error.t) result list

(* same but specialized for Target.regular *)
val map_regular_targets__run_in_forked_process_do_not_modify_globals :
  < Cap.fork > ->
  int (* ncores *) ->
  (Target.regular -> 'a) ->
  (* job function *) Target.regular list ->
  ('a, Core_error.t) result list
