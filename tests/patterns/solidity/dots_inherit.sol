//ERROR: match
contract A is B, C, D {}

//ERROR: match
contract A is C {}

//ERROR: match
contract A is C, D {}

//ERROR: match
contract A is B, C {}

contract A is B { }
