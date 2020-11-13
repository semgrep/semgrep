#include <stdio.h>

void bad_vsprintf(char *args) {
    char buffer[256];

    //ruleid: insecure-use-printf-fn
    char format[] = "%n";
    vsprintf (buffer,format, args);

    //ruleid: insecure-use-printf-fn
    vsprintf(buffer, "%n", args);

    //ok
    vsnprintf(buffer, format, args);
}

void bad_sprintf() {
    char buffer[256];
    int a = 10, b = 20, c=30;
    //ruleid: insecure-use-printf-fn
    sprintf(buffer, "Sum of %d and %d is %d", a, b, c);

    //ruleid: insecure-use-printf-fn
    char format[] = "Sum of %d and %d is %d";
    int i = 3;
    sprintf(buffer, format, a, b, c);

    //ok
    snprintf(buffer, format, a,b,c);
}

void bad_printf() {
    //ruleid: insecure-use-printf-fn
    printf("what %x", 1234);

    //ruleid: insecure-use-printf-fn
    char format[] = "what %x";
    int i = 3;
    printf(format, 1234);

    //ok
    printf("hello");
}

int main() {
    bad_vsprintf(NULL);
    bad_sprintf();
    bad_printf();
    return 0;
}
