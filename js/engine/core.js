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

//Provides: unix_environment const
function unix_environment() {
  return [];
}

//Provides: get_jsoo_mount_point
//Requires: jsoo_mount_point
function get_jsoo_mount_point() {
  return jsoo_mount_point;
}

//Provides: override_yaml_ctypes_field_offset_bytes
function override_yaml_ctypes_field_offset_bytes(field, newOffset) {
  field[2] = newOffset;
}
