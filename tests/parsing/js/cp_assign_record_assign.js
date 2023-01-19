// This used to raise `Impossible` during constant propagation because
// "_mitz" was not picked up during the `var_stats` step, yet we processed
// it later on during constant folding and we could not find "_mitz" in
// the stats table.
var [{ oy, vey: _vey, mitzvot: _mitz = 42 }] = [{}];
