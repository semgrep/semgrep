def test_equal = {
    val a = 1
    val b = 2
    //ERROR: match
    if ((a+b) == (a+b)) {
        return 1
    }
    return 0
}
