function test0() {

    let imageOrNull = null;
    [imageOrNull] = image()

    //OK:
    if (imageOrNull == null) {
        return
    }
}