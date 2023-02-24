//Provides: caml_thread_initialize
function caml_thread_initialize() {
	// noop, maybe we can look into jsoo's lwt if this is really needed someday
}

//Provides: caml_mutex_new
function caml_mutex_new() {
	// noop
}

//Provides: numcores const
function numcores() {
	return 1 // javascript, baby!
}
