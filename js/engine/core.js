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
