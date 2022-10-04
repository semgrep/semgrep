function test1() {
    //ERROR: match
    return <div attr="1" />;
}

function test12() {
    //ERROR: match
    return <nested><div attr="1" /></nested>;
}