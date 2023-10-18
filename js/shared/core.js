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
//Requires: caml_sys_open
function unix_open(path, flags, perm) {
  return caml_sys_open(path, flags, perm);
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
