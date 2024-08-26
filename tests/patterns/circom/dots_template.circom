//ERROR: match
template Test (N) {
 signal input a;
 signal output b;
 a <== N;
}

//ERROR: match
template parallel Test2 (N) {
 signal input a;
 signal output b;
 a <== N;
}

//ERROR: match
template custom Test3 (N) {
 signal input a;
 signal output b;
 a <== N;
}