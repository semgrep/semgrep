const { spawnSync } = require("node:child_process");
globalThis.processTable = new Map();
globalThis.spawnSync = spawnSync;

//Provides: unix_spawn
function unix_spawn(executable, args, optenv, usepath, redirect) {
  console.error("unix_spawn");
  console.error(executable);
  console.error(args);
  console.error(optenv);
  console.error(usepath);
  console.error(redirect);
  // This will run a process and wait for it to exit
  // TODO: Handle spawning but not waiting
  // TODO: Handle redirect and other options
  const child = globalThis.spawnSync(executable, args);
  console.error(child);
  globalThis.processTable.set(child.pid, child);
  return child.pid;
}

//Provides: unix_waitpid
function unix_waitpid(flags, pid_req) {
  console.error("unix_waitpid");
  console.error(flags);
  console.error(pid_req);
  const child = globalThis.processTable.get(pid_req);
  console.error(child);
  if (!child) {
    throw new Error(`unix_waitpid: no such process {pid_req}`);
  }
  //TODO Actually handle non exited processes
  // [type, pid, [tag,tag_value]]
  return [0, child.pid, [0, child.status]];
}

//Provides: unix_pipe
function unix_pipe(cloexec, vunit) {
  console.error("unix_pipe");
  console.error(cloexec);
  console.error(vunit);
  return [3, 4];
}

//Provides: unix_set_close_on_exec
function unix_set_close_on_exec(fd) {
  console.error("unix_set_close_on_exec");
  console.error(fd);
  return;
}

//Provides: unix_clear_close_on_exec
function unix_clear_close_on_exec(fd) {
  console.error("unix_clear_close_on_exec");
  console.error(fd);
  return;
}

//Provides: unix_dup
function unix_dup(cloexec, fd) {
  console.error("unix_dup");
  console.error(cloexec);
  console.error(fd);
  return 5;
}

//Provides: unix_close
function unix_close(fd) {
  console.error("unix_close");
  console.error(fd);
  return;
}

//Provides: unix_read
function unix_read(fd, buf, ofs, len) {
  console.error("unix_read");
  console.error(fd);
  console.error(buf);
  console.error(ofs);
  console.error(len);
  //TODO
  return 0;
}
