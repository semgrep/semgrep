val test_rules :
  ?unit_testing:bool ->
  < Core_scan.caps ; Cap.readdir ; .. > ->
  Fpath.t list ->
  unit
