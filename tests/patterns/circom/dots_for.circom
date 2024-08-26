function foo () {
    var y = 0;
    // ERROR: match
    for(var i = 0; i < 100; i++){
        y++;
    }
}