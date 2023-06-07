public class H {
    public void h() {
        RE e = null;
        e = new RE();
        e.setX(tainted);
        // ok: test
        sink(e.getY());
        // ruleid: test
        sink(e.getX());
    }
}
