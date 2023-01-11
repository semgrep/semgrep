void test() {

  // this is really ambiguous ...
  // mapfree(&rmapupa, pa, (u32int)-pa);

  // this is easier to parse also for humans
  mapfree(&rmapupa, pa, (u32int)(-pa));
}
