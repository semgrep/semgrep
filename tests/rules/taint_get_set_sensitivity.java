public class H {
    public void h() {
        RE e = null;
        e = new RE();
        e.setX(tainted);
        // ok: test
        sink(e.getY());
        // ruleid: test
        sink(e.getX());
        // ruleid: test
        sink(e.x);

        d = new RE();
        d.x = tainted;
        // OK:
        sink(d.getY());
        // ruleid: test
        sink(d.getX());
        // ruleid: test
        sink(d.x);
    }
}
