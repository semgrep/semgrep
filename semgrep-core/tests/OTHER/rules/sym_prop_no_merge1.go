func test() {
	// Before
	//
	//     79acabc303f ("dataflow: Make const-prop a true must-analysis (#4410)")
	//
	// this simple loop will make symbolic propagation introduce a cyclic
	// id_svalue that later caused a segfault. Because `cpuids` is undefined
	// before entering the loop, and locals were _|_ at the entry node, we
	// inferred that `cpuids` was the same as `append(cpuids)` at the start of
	// the loop body. After 79acabc303f we now assume that variables are
	// non-constant by default, so we do not try to infer values for variables
	// that have recursive dependencies.
	for true {
		//ruleid:test
		cpuids = append(cpuids)
	}
	// No symbolic propagation here due to for-loop
	//OK:
	return cpuids
}
