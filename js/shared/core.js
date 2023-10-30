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

//Provides: ocaml_terminal_size_get
function ocaml_terminal_size_get() {
  return [80, 120];
}

//Provides: unix_open
//Requires: caml_sys_open, caml_list_of_js_array, caml_list_to_js_array
function unix_open(path, flags, perm) {
  // Why is this needed?
  // There is a Unix.open_flag type, which is different than
  // that of the Stdlib.open_flag type.
  // As such, some flags (such as O_CREAT) appear in a different
  // index of the variant type than the corresponding Open_creat,
  // for instance.
  // So before we can call the system open command, we need to
  // translate the Unix open flags to stdlib open flags.
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
        // TODO
        return [];
      //O_DSYNC
      case 9:
        // TODO
        return [];
      //O_SYNC
      case 10:
        // TODO
        return [];
      //O_RSYNC
      case 11:
        // TODO
        return [];
      //O_SHARE_DELETE
      case 12:
        // TODO
        return [];
      //O_CLOEXEC
      case 13:
        // TODO
        return [];
      //O_KEEPEXEC
      case 14:
        // TODO
        return [];
    }
  }

  let parsed_flags = caml_list_to_js_array(flags);
  let translated_flags_nested = parsed_flags.map((flag) =>
    map_unix_flag_to_stdlib_flag(flag)
  );
  let translated_flags_flattened = translated_flags_nested.reduce((acc, l) =>
    acc.concat(l)
  );
  let new_flags = caml_list_of_js_array(translated_flags_flattened);

  return caml_sys_open(path, new_flags, perm);
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
