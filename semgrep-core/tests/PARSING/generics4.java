package com.test;

import java.util.HashMap;
import java.util.Map;

final class Test<T> {
    final Map<T, otherTest<T>> abc = new HashMap();

    Test() {
    }
}
