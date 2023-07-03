// this used to not match because of bad interaction with -fast
// and the code generated for LINK queries
//ERROR: match
from m in somelist select m.foo;
