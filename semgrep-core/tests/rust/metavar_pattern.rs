pub fn foo() {
    let mut j = 0;
    //ERROR: match
    for i in 0..10 {
        j += i;
    }
}
