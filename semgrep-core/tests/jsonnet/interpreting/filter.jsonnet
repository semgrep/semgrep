local arr = [1, 2, 3];
std.filter(function(v) v > 1, arr)

//possible errors to handle:
//std.filter(1, 2) // This should generate a runtime error

//std.filter(function() true, arr)

