type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

let start caps = Lwt_platform.run (LS.start caps)
