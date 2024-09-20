Runtime r = Runtime.getRuntime();

String command = getSafeCommand();
//OK:
r.exec(command);

String command2 = getUnsafeCommand();
//ruleid: os-command
r.exec(command2);
