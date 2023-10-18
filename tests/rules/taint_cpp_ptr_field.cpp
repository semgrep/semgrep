void test_intra_001() {
  TestObject *obj = new TestObject();

  obj->a = taint_source();
  obj->b = SAFE_STR;

  // ok: cpp-tainted-field-ptr
  sink(obj->b, __LINE__);
  // ruleid: cpp-tainted-field-ptr
  sink(obj->a, __LINE__);
}
