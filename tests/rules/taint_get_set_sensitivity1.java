public class H {
    public void h1() {
        RE e = null;
        e = new RE();
        e.setX(tainted);
        // OK: test
        sink(e.y);
        // ruleid: test
        sink(e.x);

    }
    public void h2() {
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
