#include <stdio.h>

void test_001() {
   FILE *fptr;
   fptr = fopen("filename.txt", "r");

   char buf100[100];
   // ruleid: match-simple-metavar-type
   fgets(buf100, 100, fptr);

   char buf200[200];
   // ok: type-mismatch
   fgets(buf200, 200, fptr);
}
