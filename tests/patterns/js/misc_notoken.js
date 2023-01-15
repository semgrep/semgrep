async function bar(msg) {
    // this should not match. It used to because this was rewritten
    // as let x = null, y = null; but it was bad.
    let x, y;
    y = foo(msg);
}
