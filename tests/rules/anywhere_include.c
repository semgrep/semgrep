#include "legacy-io-header.h"

int main(int argc, char **argv) {
        if (argc != 2) return 1;

        // ruleid: legacy-io
        printk("Hello, %s", argv[1]);
}
