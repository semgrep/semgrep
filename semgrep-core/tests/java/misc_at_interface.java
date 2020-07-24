public @interface abc {
    public class xyz {
        public func() {
            //ERROR: match
            asdf();
        }
    }
}
