(* Output the core results on stdout depending on flags in
 * the configuration:
 *  - Json
 *  - TODO Text
 *  - TODO Sarif
 *  - TODO ...
 *)
val output_result : Scan_CLI.conf -> Core_runner.result -> unit
