
//ERROR:
class A @AnnotX() {
    val x = 0
}

//ERROR:
class B @AnnotX @AnnotY private {
    val x = 0
}

//OK:
class C @AnnotZ private[Outer] {
    val x = 0
}