type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

let start caps =
  Logs.info (fun m -> m "Starting legacy language server");
  Lwt_platform.run (LS.start caps)
