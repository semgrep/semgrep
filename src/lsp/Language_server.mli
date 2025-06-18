type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

val start : caps -> unit
