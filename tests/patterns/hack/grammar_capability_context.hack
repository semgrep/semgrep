<?hh
// ERROR: match
function invoke((function()[io]: void) $action)[ctx $action]: void {}
function unrelated((function()[io]: void) $action)[io]: void {}
