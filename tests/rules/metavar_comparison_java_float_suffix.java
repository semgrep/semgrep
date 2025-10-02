class FloatDoubleSuffix {
    public static void test() {
        // ruleid: java-float-double-suffix-comparison
        float a = 0.5f;

        // ruleid: java-float-double-suffix-comparison
        float b = 0.5F;

        // ruleid: java-float-double-suffix-comparison
        double c = 0.5d;

        // ruleid: java-float-double-suffix-comparison
        double d = 0.5D;

        // ok: java-float-double-suffix-comparison
        float e = 1.5f;

        // ok: java-float-double-suffix-comparison
        double f = 2.0D;
    }
}
