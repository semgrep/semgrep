#include <stdio.h>

int
ok() {
    // ok
	if (0) {
		goto ONE;
		goto ONE;
    }
	printf("did not go to one\n");
	return 0;
ONE:
	printf("went to one\n");
	return 1;
}

int
main(int argc, char *argv[]) {
    // ruleid:double_goto
	if (0)
		goto ONE;
		goto ONE;
	printf("did not go to one\n");
	return 0;
ONE:
	printf("went to one\n");
	return 1;
}
