Runtime r = Runtime.getRuntime();

String command = getSafeCommand();
//OK:
r.exec(command);

String command2 = getUnsafeCommand();
//ERROR:
r.exec(command2);
