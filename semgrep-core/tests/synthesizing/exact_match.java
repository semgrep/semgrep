class SemgrepMain {
    public static void main(String[] args) {
        int a;
        return foo(bar(a)) == foo(bar(a));
    }
}

class SemgrepMain2 {
    public static void main(String[] args) {
        a == a;
        7 == 8;
        x == y;
        set_const(a) == set_const(a);
    }
}
