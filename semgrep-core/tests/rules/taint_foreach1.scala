// https://github.com/returntocorp/semgrep/issues/5652
class FooBar {
    def something(objectID: Int)(implicit ec: ExecutionContext) = {
        // ruleid: scala-taint
        val somethingbad = db.getC(false, objectID)
        for {
            a <- db.getA()
            // ruleid: scala-taint
            b <- db.getB(true, objectID, false)
            view = render(a, b)
        } yield view
    }
}