// `o?` unwraps (or returns early); it is not a null-conditional access, so
// writing through it does not taint `o` itself.
fn copy_opt(mut o: Option<P>) -> Option<()> {
    o?.f = source();
    // ok: taint
    sink(o);
    Some(())
}
