<?hh
// ERROR: match
function accepts((function()[io]: void) $f) {}
function rejects((function()[rand]: void) $f) {}
