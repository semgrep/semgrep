
function test6() {
    const foo = () => {
        var x = 1;

        // this should match
        //if (true) {
        //    console.log("hi");
        //}
    };
    // but this should not match, different scope
    console.log("hi");
}

