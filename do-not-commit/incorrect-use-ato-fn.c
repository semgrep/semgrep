#include <stdlib.h>

int main() {
    const char *buf = "";

    // ruleid:incorrect-use-ato-fn
    int i = atoi(buf);

    // ruleid:incorrect-use-ato-fn
    long j = atol(buf);

    // ruleid:incorrect-use-ato-fn
    long long k = atoll(buf);

    // ok:incorrect-use-ato-fn
    long l = strtol(buf, NULL, 10);

    // ok:incorrect-use-ato-fn
    long long m = strtol(buf, NULL, 10);

    // ok:incorrect-use-ato-fn
    long n = strtoq(buf, NULL, 10);

    return 0;
}
