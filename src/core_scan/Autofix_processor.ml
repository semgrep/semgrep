module Autofix_processor : sig
  type state

  val pre_process : Core_scan_config.t -> Rule.t list -> Rule.t list * state

  val post_process :
    Core_scan_config.t -> state -> Core_result.t -> Core_result.t
end = struct
  type state = unit

  let pre_process _config rules = (rules, ())

  let post_process (config : Core_scan_config.t) () (res : Core_result.t) =
    let new_matches = Autofix.apply_autofixes config.autofix res.matches in
    { res with matches = new_matches }
end
