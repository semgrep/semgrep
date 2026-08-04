public class ConstpropJava {

    // -----------------------------------------------------------------------
    // Operator folding: chains that produce 112 (sink(112) rule)
    // Negative inputs are fine as long as the result reaching sink is positive.
    // -----------------------------------------------------------------------

    public static void testLeftShift() {
        // 7 << 4 = 112
        int x = 7;
        int z = x << 4;
        //ruleid: test
        sink(z);
    }

    public static void testRightShift() {
        // 224 >> 1 = 112  (ASR; same as >>> for positive values)
        int x = 224;
        int z = x >> 1;
        //ruleid: test
        sink(z);
    }

    public static void testLogicalRightShift() {
        // 224 >>> 1 = 112  (LSR; same result as >> for positive values)
        int x = 224;
        int z = x >>> 1;
        //ruleid: test
        sink(z);
    }

    public static void testLsrNegativeDrivesSccp() {
        // LSR on a negative number fills high bits with 0 (result is always positive).
        // -224 >>> 1 is a large positive number; condition `> 0` is true.
        int x = -224;
        int z = x >>> 1;
        String result;
        if (z > 0) {
            result = "yes";
        } else {
            result = "no";  // dead: LSR of any non-zero value gives positive result
        }
        //proruleid: test-sccp
        sink(result);
    }

    public static void testBitAnd() {
        // 255 & 112 = 112
        int x = 255;
        int z = x & 112;
        //ruleid: test
        sink(z);
    }

    public static void testBitOr() {
        // 96 | 16 = 112
        int x = 96;
        int z = x | 16;
        //ruleid: test
        sink(z);
    }

    public static void testXorChain() {
        // (pad ^ secret) ^ pad = secret = 112
        int pad = 204;
        int secret = 112;
        int pub = pad ^ secret;       // 188
        int recovered = pub ^ pad;    // 112
        //ruleid: test
        sink(recovered);
    }

    public static void testBitNot() {
        // ~(-113) = 112  (negative input, positive result)
        int x = -113;
        int z = ~x;
        //ruleid: test
        sink(z);
    }

    public static void testArithmeticChain() {
        // 3 * 8 + 88 = 112
        int a = 3;
        int b = a * 8;   // 24
        int c = b + 88;  // 112
        //ruleid: test
        sink(c);
    }

    public static void testModResult() {
        // 312 % 200 = 112
        int x = 312;
        int z = x % 200;
        //ruleid: test
        sink(z);
    }

    // -----------------------------------------------------------------------
    // SCCP: dead-branch elimination (sink("yes") rule).
    // Used both for normal cases and for tests whose intermediate values are
    // negative (pattern stays a positive string literal).
    // -----------------------------------------------------------------------

    public static void testShiftDrivesSccp() {
        // 7 << 4 = 112; `== 112` is always true.
        int key = 7;
        int shifted = key << 4;
        String result;
        if (shifted == 112) {
            result = "yes";
        } else {
            result = "no";  // dead
        }
        //proruleid: test-sccp
        sink(result);
    }

    public static void testBitAndDrivesSccp() {
        // 5 & 6 = 4; `!= 4` is false.
        int flags = 5;
        int mask = 6;
        String result;
        if ((flags & mask) != 4) {
            result = "no";  // dead
        } else {
            result = "yes";
        }
        //proruleid: test-sccp
        sink(result);
    }

    public static void testArithmeticChainDrivesSccp() {
        // 3 * 8 - 16 = 8; `> 5` is true.
        int a = 3;
        int b = a * 8;    // 24
        int c = b - 16;   // 8
        String result;
        if (c > 5) {
            result = "yes";
        } else {
            result = "no";  // dead
        }
        //proruleid: test-sccp
        sink(result);
    }

    public static void testModDrivesSccp() {
        // 7 % 3 = 1; `== 1` is true.
        int a = 7;
        int b = a % 3;
        String result;
        if (b == 1) {
            result = "yes";
        } else {
            result = "no";  // dead
        }
        //proruleid: test-sccp
        sink(result);
    }

    // Negative-input tests: intermediate result is negative, but the sink
    // pattern is the string "yes" so the pattern stays positive.

    public static void testBitNotDrivesSccp() {
        // ~42 = -43; `-43 < 0` is true.
        int x = 42;
        int z = ~x;   // -43
        String result;
        if (z < 0) {
            result = "yes";
        } else {
            result = "no";  // dead
        }
        //proruleid: test-sccp
        sink(result);
    }

    public static void testNegativeDivisionDrivesSccp() {
        // -8 / 2 = -4 (exact, so folds); `-4 < 0` is true.
        int x = -8;
        int z = x / 2;
        String result;
        if (z < 0) {
            result = "yes";
        } else {
            result = "no";  // dead
        }
        //proruleid: test-sccp
        sink(result);
    }

    public static void testNegativeSubtractionDrivesSccp() {
        // 3 - 10 = -7; `-7 != 0` is true.
        int a = 3;
        int b = 10;
        int diff = a - b;   // -7
        String result;
        if (diff != 0) {
            result = "yes";
        } else {
            result = "no";  // dead
        }
        //proruleid: test-sccp
        sink(result);
    }

    // -----------------------------------------------------------------------
    // Loop-invariant propagation
    // -----------------------------------------------------------------------

    public static void testLoopInvariantInt() {
        // x is never written in the loop; it remains 112 after the loop.
        int x = 112;
        int total = 0;
        for (int i = 0; i < 10; i++) {
            total += i;
        }
        //proruleid: test
        sink(x);
    }

    public static void testLoopInvariantDrivesSccp() {
        // x is constant before and after the loop; condition evaluable by SCCP.
        int x = 42;
        int sum = 0;
        for (int i = 0; i < 5; i++) {
            sum += i;
        }
        String result;
        if (x == 42) {
            result = "yes";
        } else {
            result = "no";  // dead
        }
        //proruleid: test-sccp
        sink(result);
    }

    // -----------------------------------------------------------------------
    // No fold on non-constant inputs
    // -----------------------------------------------------------------------

    public static void testNonConstant(int n) {
        int z = n & 112;
        //ok: test
        sink(z);
    }

    public static void testNonConstantSccp(int n) {
        String result;
        if (n == 112) {
            result = "yes";
        } else {
            result = "no";
        }
        // OK: n is not constant.
        sink(result);
    }
}
