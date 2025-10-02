class LiteralFloatSuffix {
    public static void test() {
        // MATCH:
        float a = 0.5f;
        // MATCH:
        float b = 0.5F;
        // MATCH:
        double c = 0.5d;
        // MATCH:
        double d = 0.5D;
        // MATCH:
        double e = 0.5;
        float f = 123.456f;
        double g = 123.456D;
        double h = 1.0;
    }
}
