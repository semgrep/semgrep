func test() {
	for true {
		//ruleid:test
		cpuids = append(cpuids)
	}
	// No symbolic propagation here due to for-loop
	//OK:
	return cpuids
}
