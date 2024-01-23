/* What is this file for?
   We would like to transpile our OCaml code to JS, to make it runnable in
   the browser and on Windows systems. The OCaml standard library, however,
   has external C dependencies, some of which are unimplemented by the JSOO
   transpiler.
   In particular, the Unix module consists largely of external C calls which
   are not defined symbols in the produced JS.
   To make sure we can still call those C functions (which happens in things
   such as Bos, Time_limit, etc), we reimplement those fundamental Unix calls
   in Javascript.
   Note that this doesn't need to be a perfect reimplementation, but we can
   get by with "good enough" behavior.
 */

const { spawnSync } = require("node:child_process");
const os = require("os");
const fs = require("fs");
const process = require("process");

globalThis.spawnSync = spawnSync;
globalThis.fs = fs;
globalThis.os = os;
globalThis.process = process;

// mapping pid to its own process
globalThis.pidToProcessTable = new Map();
// mapping pid of parent to its children
globalThis.pidToChildrenTable = new Map();
/* NOTE(fake-fds): Why do we have this idea of "fake" file descriptors?
   Values of the `file_descriptor` type can arise from a few places. Two such
   are `caml_sys_open`, which is implemented by jsoo, and `unix_pipe`, which
   is implemented below.
   A problem is that we don't want to mess with file descriptors when implementing
   `unix_pipe` or `unix_spawn` -- ideally, we could use Node's redirection logic
   and simply read the data internally from JS, instead of writing out to a file
   explicitly.
   As such, when we use `unix_pipe`, we want to generate "fake file descriptors",
   which simply biject to `unix_spawn` calls, and can be used as keys to lookup
   the output of those processes.
   By our types, these fake file descriptors must be integers, however, and they
   must contend with the fact that `caml_sys_open` will also generate file
   descriptors, by just increasing indices.
   At the point of calling `unix_read`, we need to know whether we are holding a
   "real" file descriptor (from `caml_sys_open`), or a "fake" file descriptor
   (from `unix_pipe`). To make sure there are no collisions, we are just going to
   start our fake file descriptors at 20,000, and assume that any file descriptors
   numbered greater than that are fake.
*/
// There will be collisions if we ever allocate 20000 or more file descriptors
// "naturally", that is, through `caml_sys_open`, but this should be unlikely.
// According to Austin: should be Ok, because node uses libuv
// https://www.geeksforgeeks.org/libuv-in-node-js/
globalThis.fakeFdThreshold = 20000;
globalThis.fdCount = globalThis.fakeFdThreshold;
globalThis.fdTable = new Map();

/* Some magic happens here, in conjunction with unix_pipe.
   Read "NOTE(fake-fds)" for more.
   The basic TL;DR is we generate fake file descriptors when piping or duping,
   which we then associate to the output of this process, using Node's inherent
   IO redirection capabilities.
   Then, when reading from this "file descriptor", we just get the associated
   output.
 */
//Provides: unix_spawn
function unix_spawn(executable, args, optenv, usepath, redirect) {
  // get rid of the mandatory tag and the first argument, which is the
  // executable path
  let argv = args.slice(2);

  /* Since we only need to implement what is needed to make Bos work with our
     git commands, we do not currently specially treat stdin/stdout/stderr
     whatsoever.
     This is OK for now, but is a deliberate decision by us to leave this as a
     TODO. Future work may entail implementing this.
   */

  // See "NOTE(fake-fds)"
  // These redirects are given to `unix_spawn` by Bos via `Unix.create_process_gen`,
  // which gets them from Bos explicitly calling `pipe`.
  // Via our convention in unix_pipe, these are not actually "real" file descriptors,
  // but fresh identifiers we will use to map to the output of the corresponding
  // child process.
  // As such, let's just use the `pipe` setting for the stdio of this child process.
  let output_fd = redirect[2];

  /* In Javascript, there is no `execvp` and `execv` which
     switch between searching in the PATH or not.
     We will just run the vanilla `exec`, which should behave
     as an `execvp`.
     This might have slightly differing behavior than the
     actual implementation.
   */
  if (optenv == 0) {
    const child = globalThis.spawnSync(executable, argv, { stdio: "pipe" });

    globalThis.pidToProcessTable.set(child.pid, child);
    globalThis.fdTable.set(output_fd, child.stdout);

    return child.pid;
  }
  let env = {};
  for (const arg of optenv[1].slice(1)) {
    const idx = arg.indexOf("=");
    switch (idx) {
      case -1:
        console.debug(
          "unix_spawn: given optenv argument without '=' character"
        );
      default:
        const key = arg.slice(0, idx);
        const value = arg.slice(idx + 1);
        env[key] = value;
    }
  }
  const child = globalThis.spawnSync(executable, argv, {
    env: env,
    stdio: "pipe",
  });

  globalThis.pidToProcessTable.set(child.pid, child);
  globalThis.fdTable.set(output_fd, child.stdout);

  let current_pid = globalThis.process.pid;

  // associate parent to new child
  if (globalThis.pidToChildrenTable.has(current_pid)) {
    globalThis.pidToChildrenTable.set(
      current_pid,
      globalThis.pidToChildrenTable.get(current_pid).concat([child])
    );
  } else {
    globalThis.pidToChildrenTable.set(current_pid, [child]);
  }

  return child.pid;
}

//Provides: unix_waitpid
function unix_waitpid(flags, pid_req) {
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
    let children = globalThis.pidToChildrenTable.get(process.pid);
    if (!children) {
      throw new Error(`unix_waitpid: no children process of parent {pid_req}`);
    }
    for (const child of children) {
      return handleChild(child);
    }
    // specific process
  } else if (pid_req > 0) {
    let child = globalThis.pidToProcessTable.get(pid_req);
    if (!child) {
      throw new Error(`unix_waitpid: no child at pid {pid_req}`);
    }

    // When reaping a parent, its children become orphaned, and
    // have their parent set to a system-defined process.
    // Often, this is 1.
    // TODO: For now, let's just delete it altogether and worry about
    // the precise parenting details later.
    globalThis.pidToChildrenTable.delete(pid_req);
    globalThis.pidToProcessTable.delete(pid_req);

    return handleChild(child);
    // process group nonsense
  } else {
    function getAllDescendants(pid) {
      let children = globalThis.pidToChildrenTable.get(pid);
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
  // See "NOTE(fake-fds)"
  // The pipe we spawn here will eventually reach unix_spawn.
  // We don't want to use a real file descriptor, bceause we're trying
  // to avoid them, in favor of Node's redirection ability, which will open
  // the corresponding file descriptors for us.
  // Because this call requires us to decide on what the file descriptor is
  // ahead of time, though, we will just pass around a simulated FD, which
  // in conjunction with fdTable, will let us recover the content which is
  // supposed to be written to that FD.
  let fd = globalThis.fdCount;

  globalThis.fdCount++;

  return [0, fd, fd];
}

//Provides: unix_set_close_on_exec
function unix_set_close_on_exec(fd) {
  return;
}

//Provides: unix_getcwd
//Requires: caml_sys_getcwd
function unix_getcwd(vunit) {
  return caml_sys_getcwd(vunit);
}

//Provides: unix_chdir
//Requires: caml_sys_chdir
function unix_chdir(path) {
  caml_sys_chdir(path);

  return;
}

//Provides: unix_clear_close_on_exec
function unix_clear_close_on_exec(fd) {
  return;
}

//Provides: unix_dup
//Requires: unix_pipe
function unix_dup(cloexec, fd) {
  // We just use the same unix_pipe logic to generate a fresh fake
  // file descriptor.
  return unix_pipe(cloexec, fd);
}

//Provides: unix_close
function unix_close(fd) {
  return;
}

//Provides: unix_fork
function unix_fork(vunit) {
  return;
}

//Provides: unix_setitimer
function unix_setitimer(it, its) {
  // In our code, the result value is just ignored, so we can return the same one.
  // TODO: Theoretically this is supposed to do some SIGALRM stuff too, but
  // that is complicated, so let's leave it as a TODO.
  return its;
}

// TODO
//Provides: unix_sleep
function unix_sleep(duration) {
  // You cannot actually sleep synchronously, so let's just leave this
  // as a TODO.

  return;
}

// TODO: This is fixed as of this PR:
// https://github.com/ocsigen/js_of_ocaml/pull/1519
// so when this is released, we will just need to upgrade our jsoo version
//Provides: caml_unix_lstat_64
//Requires: caml_unix_lstat, caml_int64_of_int32
//Alias: unix_lstat_64
function caml_unix_lstat_64(name) {
  var r = caml_unix_lstat(name);
  r[9] = caml_int64_of_int32(r[9]);
  return r;
}

//Provides: unix_realpath
function unix_realpath(path) {
  return globalThis.fs.realpathSync(path);
}

//Provides: unix_umask
function unix_umask(newmask) {
  let oldmask = globalThis.process.umask(newmask);
  return oldmask;
}

//Provides: unix_write
//Requires: caml_sys_fds
function unix_write(fd, buf, ofs, len) {
  let file = caml_sys_fds[fd];

  // The reason why this gymnastics is needed, instead of just calling file.write
  // on the buffer buf.c, is because for some reason the call to writeSync is
  // copying all of the bytes of the buffer, instead of the requested length from
  // the offset.
  // I observed previously that, for a 1024 byte buffer, it would copy all 1024
  // bytes to the target, instead of the requested 178.
  // So we just construct a new buffer of what we really want, to ensure the call
  // gets it right.
  let bufferSlice = buf.c.slice(ofs, ofs + len);

  // The implementation of MlNodeFd.write is such that
  // this always returns 0 if this succeeds.
  // So let's ignore it.
  let bytesWritten = file.write(null, bufferSlice, 0, bufferSlice.length);

  return bufferSlice.length;
}

//Provides: unix_read
//Requires: caml_sys_fds, caml_convert_bytes_to_array
function unix_read(fd, buf, ofs, len) {
  // buf is of type `bytes` in OCaml, which in JSCaml is
  // represented by an MlBytes object

  // This will ensure buf.c is an array.
  // We only need to do this if it's not already an array, though.
  if (buf.t != 4) caml_convert_bytes_to_array(buf);

  // We are just using `UNIX_BUFFER_SIZE` because that's what the original implementation does.
  // https://github.com/ocaml/ocaml/blob/6bad032104aa3f03e5aba922acc6bd9f7835bf91/otherlibs/unix/read_unix.c#L28
  const UNIX_BUFFER_SIZE = 65536;
  let length = len <= UNIX_BUFFER_SIZE ? len : UNIX_BUFFER_SIZE;

  // We do the things we do here because we want a new buffer of the
  // truncated contents of `buf`, up to the UNIX_BUFFER_SIZE.
  // We will read the output we are trying to consume into this buffer,
  // then copy it back to the MlBytes object.
  const buffer = Buffer.alloc(length);
  const from_buffer = Buffer.from(buf.c);
  from_buffer.copy(buffer, 0, 0, length);

  if (fd >= globalThis.fakeFdThreshold) {
    // See "NOTE(fake-fds)"
    // Via our protocol, this is a unique identifier associated to the
    // pipe (and by extension, the spawned process), which is mapped to
    // the corresponding child.
    // Because the child was spawned via 'pipe' stdio, we should be able
    // to just read the child's output fd to get the data.
    if (!globalThis.fdTable.has(fd)) {
      /* This is not an error case.
         Because we synchronously read all the output, we return all the
         data at once.
         The code using `Unix.read`, however, is going to repeatedly read
         until EOF.
         So, our protocol is that on the first read, we return all the data
         and remove the fd from the table. On the second read, there is no
         fd, so we just signal that we are done reading.
       */
      return 0;
    }

    let result = globalThis.fdTable.get(fd);

    let resultBuffer = typeof result == "string" ? Buffer.from(result) : result;

    // THINK: Problem if the amount of bytes requested is greater than the buffer?
    let bytesRead = resultBuffer.copy(buffer, ofs, 0, length);

    buffer.copy(buf.c, 0);

    globalThis.fdTable.delete(fd);

    return bytesRead;
  } else {
    let file = caml_sys_fds[fd];
    // THINK: Problem if the amount of bytes requested is greater than the buffer?
    let bytesRead = file.read(null, buffer, ofs, length);

    buffer.copy(buf.c, 0);

    return bytesRead;
  }
}

//Provides: unix_rename
function unix_rename(path1, path2) {
  globalThis.fs.renameSync(path1, path2);
}
