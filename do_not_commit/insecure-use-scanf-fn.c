#include <stdio.h>

int DST_BUFFER_SIZE = 120;

int bad_code() {
    char str[DST_BUFFER_SIZE];
    // ruleid:insecure-use-scanf-fn
    scanf("%s", str);
    printf("%s", str);
    return 0;
}

int main() {
    char str[DST_BUFFER_SIZE];
    // ok
    fgets(str);
    printf("%s", str);
    return 0;
}
