function foo() {
    if (a == 2) return 1;
    else return 2;
}

//ERROR: 
function foo() {
    if (a == 2) return "efg";
    else return "abc";
}
