val map_targets :
  conf:Parallelism_config.t ->
  num_jobs:int ->
  (Target.t -> 'a) ->
  Target.t list ->
  ('a, exn) result list
(** [map_targets] basically wraps {!Concurrent.map} but with some extra Semgrep
    specific tweaks *)
