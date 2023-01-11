// avoid introduce too many classes. ADT on non-steroids
class X {
    enum Bound {
       INCLUSIVE {
            @Override public String leftCap(Object from) {
                return "[" + from;
            }
            @Override public String rightCap(Object to) {
                return to + "]";
            }
        },
        EXCLUSIVE {
            @Override public String leftCap(Object from) {
                return "(" + from;
            }
            @Override public String rightCap(Object to) {
                return to + ")";
            }
        };

        public abstract String leftCap(Object from);
        public abstract String rightCap(Object to);
    }
}
