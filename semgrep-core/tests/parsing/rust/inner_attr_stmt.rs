impl Drop for PerfMap {
    fn drop(&mut self) {
	// this used to cause some parse errors
        #![allow(unused_must_use)]
        perf_event_ioc_disable(self.ev_fd);
        unsafe {
            libc::close(self.ev_fd);
        }
    }
}
