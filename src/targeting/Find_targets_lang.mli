(* This is essentially [Find_targets.get_target_fpaths] combined with
 * [Filter_target.filter_target_for_analyzer].
 *
 * This should be used mostly in testing code to quickly get a list
 * of targets for a certain language. This internally relies on
 * [Find_targets.conf.force_project_root] being set to the [root] parameter
 * (so one can use this function on test paths like "tests/tainting/"
 * and the targets will not be filtered by the toplevel .gitignore
 * or .semgrepignore of the repo containing those tests).
 *)
val get_target_fpaths :
  < Cap.readdir ; .. > -> Fpath.t (* root *) -> Lang.t -> Fpath.t list
