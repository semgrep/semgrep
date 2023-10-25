const { spawn, spawnSync } = require("node:child_process");
const os = require("os");
const fs = require("fs");
const stream = require("node:stream");

globalThis.processTable = new Map();
// mapping parent to children
globalThis.childTable = new Map();
globalThis.spawnSync = spawnSync;
globalThis.spawn = spawn;
globalThis.fs = fs;
globalThis.os = os;
globalThis.stream = stream;
// There will be collisions if we ever allocate 20000 or more file descriptors
// "naturally", that is, through `caml_sys_open`.
globalThis.fdCount = 20000;
globalThis.fdTable = new Map();

//Provides: unix_spawn
function unix_spawn(executable, args, optenv, usepath, redirect) {
  console.error("unix_spawn");
  console.error(executable);
  console.error(args);
  console.error(optenv);
  console.error(usepath);
  console.error(redirect);

  let argv = args.slice(2);

  // These redirects are given to `unix_spawn` by Bos via `Unix.create_process_gen`,
  // which gets them from Bos explicitly calling `pipe`.
  // This is so that the output of each child process is distinct from that of its
  // parent. We're not creating different pipes for the sake of it, so much as just
  // separating out their outputs.
  // As such, let's just use the `pipe` setting for the stdio of this child process.
  redirect = redirect.slice(1);

  /* The redirection logic is taken care of by the stdio: redirect
     business down below.
   */

  /* In Javascript, there is no `execvp` and `execv` which
     switch between searching in the PATH or not.
     We will just run the vanilla `exec`, which should behave
     as an `execvp`.
     This might have slightly differing behavior than the
     actual implementation.
   */
  if (optenv == 0) {
    const child = globalThis.spawnSync(executable, argv, { stdio: "pipe" });

    console.error(
      `Have spawned child ${child.pid} with fd ${redirect[1]} for executable ${executable} ${argv}`
    );

    globalThis.processTable.set(child.pid, child);
    globalThis.fdTable.set(redirect[1], child.stdout);

    console.error(`Got output ${child.stdout}`);

    return child.pid;
  } else {
    let env = {};
    for (const arg of optenv[1].slice(1)) {
      const [key, value] = arg.split("=");
      env[key] = value;
    }
    const child = globalThis.spawnSync(executable, argv, {
      env: env,
      stdio: "pipe",
    });

    console.error(
      `Have spawned child ${child.pid} with fd ${redirect[1]} for executable ${executable} ${argv}`
    );

    globalThis.processTable.set(child.pid, child);
    globalThis.fdTable.set(redirect[1], child.stdout);

    console.error(`Got output ${child.stdout}`);

    // associate parent to new child
    if (globalThis.childTable.has(process.pid)) {
      globalThis.childTable.set(
        process.pid,
        globalThis.childTable.get(process.pid).concat([child])
      );
    } else {
      globalThis.childTable.set(process.pid, [child]);
    }

    return child.pid;
  }
}

//Provides: unix_waitpid
function unix_waitpid(flags, pid_req) {
  console.error("unix_waitpid");
  console.error(flags);
  console.error(pid_req);

  // TODO: flags stuff is not implemented
  // TODO: WIFSTOPPED is not currently possible, because I don't know how to
  // tell if a Javascript process is stopped or killed

  function handleChild(child) {
    // if it received a signal and is no longer running
    // let's assume it was killed by the signal
    let pid = child.pid;

    if (child.status === null) {
      // WSIGNALED of int
      let status = [1, globalThis.os.constants.signals[child.signal]];
      return [0, pid, status];
    } else {
      // WEXITED of int
      let status = [0, child.status];
      return [0, pid, status];
    }
  }

  // any child process
  if (pid_req == -1) {
    let children = globalThis.childTable.get(process.pid);
    if (!children) {
      throw new Error(`unix_waitpid: no children process of parent {pid_req}`);
    }
    for (const child of children) {
      return handleChild(child);
    }
    // specific process
  } else if (pid_req > 0) {
    let child = globalThis.processTable.get(pid_req);
    if (!child) {
      throw new Error(`unix_waitpid: no child at pid {pid_req}`);
    }

    return handleChild(child);
    // process group nonsense
  } else {
    function getAllDescendants(pid) {
      let children = globalThis.childTable.get(pid);
      if (!children) {
        return [pid];
      }

      let acc = [];
      for (const child of children) {
        acc = acc.concat(getAllDescendants(child.pid));
      }
      return [pid].concat(acc);
    }

    let descendants = getAllDescendants(process.pid);
    for (const child of descendants) {
      return handleChild(child);
    }
  }
}

//Provides: unix_pipe
function unix_pipe(cloexec, vunit) {
  console.error("unix_pipe");
  console.error(cloexec);
  console.error(vunit);

  let fd = globalThis.fdCount;

  globalThis.fdCount++;

  return [0, fd, fd];

  // These aren't real file descriptors, but they're integers which map to
  // our simulated pipe.
  let fd1 = globalThis.fdCount;
  let fd2 = globalThis.fdCount + 1;

  let stream = new globalThis.stream.Duplex();

  globalThis.fdTable.set(fd1, stream);
  globalThis.fdTable.set(fd2, stream);

  globalThis.fdCount = globalThis.fdCount + 2;

  return [0, fd1, fd2];
}

//Provides: unix_set_close_on_exec
function unix_set_close_on_exec(fd) {
  console.error("unix_set_close_on_exec");
  console.error(fd);
  return;
}

//Provides: unix_getcwd
//Requires: caml_sys_getcwd
function unix_getcwd(vunit) {
  console.error("unix_cwd");

  return caml_sys_getcwd(vunit);
}

//Provides: unix_chdir
//Requires: caml_sys_chdir
function unix_chdir(path) {
  console.error("unix_chdir");
  console.error(path);

  var x = require("child_process");

  caml_sys_chdir(path);

  return;
}

//Provides: unix_clear_close_on_exec
function unix_clear_close_on_exec(fd) {
  console.error("unix_clear_close_on_exec");
  console.error(fd);
  return;
}

//Provides: unix_dup
//Requires: unix_pipe
function unix_dup(cloexec, fd) {
  console.error("unix_dup");
  console.error(cloexec);
  console.error(fd);

  // This is because when
  return unix_pipe(cloexec, fd);
}

//Provides: unix_close
function unix_close(fd) {
  console.error("unix_close");
  console.error(fd);

  return;
}

//Provides: unix_fork
function unix_fork(vunit) {
  console.error("unix_fork");
  console.error(vunit);

  return;
}

// TODO
//Provides: unix_setitimer
function unix_setitimer(it, its) {
  console.error("unix_setitimer");
  console.error(it);
  console.error(its);

  // In our code, this is just ignored, so we can return the same one.
  return its;
}

//Provides: unix_sleep
function unix_sleep(duration) {
  console.error("unix_sleep");
  console.error(duration);

  // In our code, this is just ignored, so we can return the same one.
  return;
}

// TODO: We should not have to do this.
//Provides: caml_unix_lstat_64
//Requires: caml_unix_lstat, caml_int64_of_int32
//Alias: unix_lstat_64
function caml_unix_lstat_64(name) {
  var r = caml_unix_lstat(name);
  r[9] = caml_int64_of_int32(r[9]);
  return r;
}

//Provides: unix_write
//Requires: caml_sys_fds
function unix_write(fd, buf, ofs, len) {
  console.error("unix_write");
  console.error(fd);
  console.error(ofs);
  console.error(len);

  let file = caml_sys_fds[fd];
  // The implementation of MlNodeFd.write is such that
  // this always returns 0 if this succeeds.
  // Let's just assume we wrote the right number of bytes.
  let bytesWritten = file.write(null, buf.c, ofs, len);

  return len;
}

//Provides: unix_read
//Requires: caml_sys_fds
function unix_read(fd, buf, ofs, len) {
  // buf is of type `bytes` in OCaml, which in JSCaml is
  // represented by an MlBytes object
  console.error("unix_read");
  console.error(fd);
  // console.error(buf);
  console.error(ofs);
  console.error(len);

  const UNIX_BUFFER_SIZE = 65536;
  let length = len <= UNIX_BUFFER_SIZE ? len : UNIX_BUFFER_SIZE;

  // This is a new buffer of the truncated contents of `buf`, up to
  // the UNIX_BUFFER_SIZE.
  // We will read the output we are trying to consume into this buffer,
  // then copy it back to the MlBytes object.
  const buffer = Buffer.alloc(UNIX_BUFFER_SIZE);
  buffer.write(buf.c, 0, length);

  if (fd >= 20000) {
    // Via our protocol, this is a unique identifier associated to the
    // pipe (and by extension, the spawned process), which is mapped to
    // the corresponding child.
    // Because the child was spawned via 'pipe' stdio, we should be able
    // to just read the child's output fd to get the data.
    if (!globalThis.fdTable.has(fd)) {
      console.error(`fd ${fd} not in fdTable`);
      return 0;
    }

    let result = globalThis.fdTable.get(fd);

    console.error(`got result from fd ${fd}`);

    let resultBuffer;
    if (typeof result == "string") {
      resultBuffer = Buffer.from(result);
    } else {
      resultBuffer = result;
    }

    let bytesRead = resultBuffer.copy(buffer, ofs, 0);

    console.error(`bytesRead are ${bytesRead}`);

    // then copy it back to the object. The length should remain the same.
    buf.c = buffer.toString("utf8", 0, length);

    globalThis.fdTable.delete(fd);

    return bytesRead;
  } else {
    console.error(`want to readSync from real fd ${fd}`);

    // This may not actually be correct. These fds are spawned via the
    // caml system, in particular, caml_sys_open. These don't ever call
    // the actual filesystem primitives, they instead use jsoo's own
    // code for simulating the file system.
    // let bytesRead = globalThis.fs.readSync(fd, buffer, ofs, len, null)

    let file = caml_sys_fds[fd];
    let bytesRead = file.read(null, buffer, ofs, len);

    console.error(`bytesRead from fd ${fd} are ${bytesRead}`);

    buf.c = buffer.toString("utf8", 0, length);

    return bytesRead;
  }
}
