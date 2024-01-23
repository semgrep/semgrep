//Provides: caml_thread_initialize const
function caml_thread_initialize() {
  // noop, maybe we can look into jsoo's lwt if this is really needed someday
}

//Provides: caml_mutex_new const
function caml_mutex_new() {
  // noop
}

//Provides: numcores const
function numcores() {
  return 1; // javascript, baby!
}


// js_of_ocaml assumes that if the platform is win32, then it returns Cygwin.
// But we want it to be actual Windows.

//Provides: os_type
var os_type = (globalThis.process &&
               globalThis.process.platform &&
               globalThis.process.platform == "win32") ? "Win32" : "Unix";


// override so we use the above os_type
// Provides: caml_sys_get_config const
//Requires: caml_string_of_jsbytes, os_type
function caml_sys_get_config () {
  return [0, caml_string_of_jsbytes(os_type), 32, 0];
}

//Provides: ocaml_terminal_size_get
function ocaml_terminal_size_get() {
  return [80, 120];
}

//Provides: unix_open
//Requires: caml_sys_fds, caml_list_of_js_array, caml_list_to_js_array, resolve_fs_device, MlNodeFd, MlNodeDevice, caml_create_bytes, caml_raise_no_such_file, caml_raise_sys_error
function unix_open(path, flags, perm) {
  // Why is this needed?
  // There is a Unix.open_flag type, which is different than
  // that of the Stdlib.open_flag type.
  // As such, some flags (such as O_CREAT) appear in a different
  // index of the variant type than the corresponding Open_creat,
  // for instance.
  // So before we can call the system open command, we need to
  // translate the Unix open flags to stdlib open flags.
  function todo(flag) {
    console.debug(`TODO: map unix flag ${flag} to stdlib flag`);
    return [];
  }

  function map_unix_flag_to_stdlib_flag(flag) {
    switch (flag) {
      // O_RDONLY
      case 0:
        return [0];
      // O_WRONLY
      case 1:
        return [1];
      // O_RDWR
      case 2:
        return [0, 1];
      //O_NONBLOCK
      case 3:
        return [8];
      //O_APPEND
      case 4:
        return [2];
      //O_CREAT
      case 5:
        return [3];
      //O_TRUNC
      case 6:
        return [4];
      //O_EXCL
      case 7:
        return [5];
      //O_NOCTTY
      case 8:
        return todo(flag);
      //O_DSYNC
      case 9:
        return todo(flag);
      //O_SYNC
      case 10:
        return todo(flag);
      //O_RSYNC
      case 11:
        return todo(flag);
      //O_SHARE_DELETE
      case 12:
        return todo(flag);
      //O_CLOEXEC
      case 13:
        return todo(flag);
      //O_KEEPEXEC
      case 14:
        return todo(flag);
    }
  }

  /* Produce the new flags, in the Stdlib.open_flag type */
  let parsed_flags = caml_list_to_js_array(flags);
  let translated_flags_nested = parsed_flags.map((flag) =>
    map_unix_flag_to_stdlib_flag(flag)
  );
  let translated_flags_flattened = translated_flags_nested.reduce((acc, l) =>
    acc.concat(l)
  );
  let new_flags = caml_list_of_js_array(translated_flags_flattened);

  /* OK, here some weird stuff is happening.
     Ideally, we could just hand everything off to caml_sys_open, and call
     it a day.
     Unfortunately, the APIs of Unix and Stdlib differ slightly. Not only
     are their types of open_flag in a different order, but Unix.open_flag
     supports O_RDWR, and Stdlib.open_flag has no corresponding variant.
     This ultimately comes out of the fact that Stdlib only supports two
     explicit functions which open out channels and in channels respectively,
     and do not need to open one which can do both.
     This is fine for the OCaml API, but disastrous for us, because we're
     trying to piggy-back off the Stdlib implementation, which cannot do
     both read and write.
     Looking at the jsoo code for these file devices, however, there is no
     reason why it _can't_ do both reading and writing at the same time,
     in principle, but it has checks which raise exceptions when it detects
     both Open_rdonly and Open_wronly, to be in line with the OCaml Stdlib
     behavior.
     To avoid having to implement all this virtual simulated file system
     nonsense, we can still piggy-back off the caml_sys_open impl, but we
     need to duck underneath the checks for conflicting flags.
     Below, this code inlines the majority of the body of caml_sys_open,
     but without the bad checks. We expect that this code should not really
     change in the future.
   */

  /* inlined from caml_sys_open */
  var f = {};
  while (new_flags) {
    switch (new_flags[1]) {
      case 0:
        f.rdonly = 1;
        break;
      case 1:
        f.wronly = 1;
        break;
      case 2:
        f.append = 1;
        break;
      case 3:
        f.create = 1;
        break;
      case 4:
        f.truncate = 1;
        break;
      case 5:
        f.excl = 1;
        break;
      case 6:
        f.binary = 1;
        break;
      case 7:
        f.text = 1;
        break;
      case 8:
        f.nonblock = 1;
        break;
    }
    new_flags = new_flags[2];
  }

  /* NOTE(bad-open):
    So far, by inlining, we have avoided one check for the conflicting flags.
    Unfortunately, the next call to root.device.open has one more, and it's a
    lot of effort to go to inlining it, because there's a lot of object
    properties it relies on.
    However, root.device.open doesn't actually really care about the wronly
    and rdonly flags, except to check for the conflict, and it passes back an
    object which has the final flags set inside of it (for future use).
    So what we can do is remove one of the flags, so as to remove the conflict,
    and then set it in the resulting MlFakeFd object, as if nothing happened.
   */
  let is_wronly = false;
  if (f.wronly) {
    f.wronly = 0;
    is_wronly = true;
  }

  let file;
  /* inlined from caml_sys_open */

  // Node is weird and doesn't like opening NUL on Windows
  // if its fs.openSync("NUL", "rs+") call, so we have to
  // special case it and use \\\\.\\nul instead.
  if (globalThis.process.platform === "win32" && path === "NUL") {
    path = "\\\\.\\nul";
    const device = new MlNodeDevice("");
    const fd = device.fs.openSync(path, "rs+");
    file = new MlNodeFd(fd, f);
  }else{
    var root = resolve_fs_device(path);
    file = root.device.open(root.rest, f);
  }

  /* see NOTE(bad-open) */
  if (is_wronly) {
    file.flags.wronly = 1;
  }

  /* inlined from caml_sys_open_internal */
  let idx = caml_sys_fds.length;
  caml_sys_fds[idx] = file;
  return idx | 0;
}

//Provides: unix_environment const
function unix_environment() {
  return [];
}

//Provides: set_jsoo_mountpoint
//Requires: jsoo_mount_point
function set_jsoo_mountpoint(value) {
  jsoo_mount_point = value;
}

//Provides: get_jsoo_mountpoint
//Requires: jsoo_mount_point
function get_jsoo_mountpoint() {
  return jsoo_mount_point;
}

//Provides: override_yaml_ctypes_field_offset_bytes
function override_yaml_ctypes_field_offset_bytes(field, newOffset) {
  field[2] = newOffset;
}
