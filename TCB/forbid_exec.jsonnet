//TODO:
// - Unix.open_process_in (and UUnix.open_process_in)
// - Bos.OS.Cmd
// - Feather library
// - ...
// - UCmd.cmd_to_list
// - UCmd.xxx_of_run, UCmd.with_open_process_in

{
  rules: [
    {
      id: 'forbid-exec',
      match: { any: [
	'Sys.command ...',
	'USys.command ...',
      ] },
      languages: ['ocaml'],
      paths: {
        exclude: ['tools/*', 'scripts/*', 'Test*.ml'],
      },
      severity: 'ERROR',
      message: |||
        Do not invoke directly external commands. Use the safer CapExec.ml module.
      |||,
    },
  ],

}
